{-# LANGUAGE CPP, ScopedTypeVariables #-}
{- | core functions and types for HTTPS support
-}
module Happstack.Server.Internal.Cryptonite.TLS where

import Control.Concurrent                         (forkIO, killThread, myThreadId)
import Control.Exception.Extensible               as E
import Control.Monad                              (forever, when)
import Crypto.Random.EntropyPool
import Data.Default.Class
import Data.Time                                  (UTCTime)
import GHC.IO.Exception                           (IOErrorType(..))
import Happstack.Server.Internal.Listen           (listenOn)
import Happstack.Server.Internal.Handler          (request)
import Happstack.Server.Internal.Socket           (acceptLite)
import Happstack.Server.Internal.TimeoutManager   (cancel, initialize, register)
import Happstack.Server.Internal.Cryptonite.TimeoutSocketTLS as TSS
import Happstack.Server.Internal.Types            (Request, Response)
import Network.Socket                             (HostName, PortNumber, Socket, sClose, socketPort)
import Network.TLS
import Network.TLS.Extra.Cipher
import Happstack.Server.Types                     (LogAccess, logMAccess)
import System.IO.Error                            (ioeGetErrorType, isFullError, isDoesNotExistError)
import System.Log.Logger                          (Priority(..), logM)
#ifndef mingw32_HOST_OS
import System.Posix.Signals                       (Handler(Ignore), installHandler, openEndedPipe)
#endif


-- | wrapper around 'logM' for this module
log':: Priority -> String -> IO ()
log' = logM "Happstack.Server.Internal.TLS"


-- | configuration for using https:\/\/
data TLSConf = TLSConf {
      tlsPort      :: Int        -- port (usually 443)
    , tlsCert      :: FilePath   -- path to SSL certificate
    , tlsKey       :: FilePath   -- path to SSL private key
    , tlsCA        :: Maybe FilePath -- PEM encoded list of CA certificates
    , tlsTimeout   :: Int        -- kill connect of timeout (in seconds)
    , tlsLogAccess :: Maybe (LogAccess UTCTime) -- see 'logMAccess'
    , tlsValidator :: Maybe (Response -> IO Response) -- ^ a function to validate the output on-the-fly
    }

-- | a partially complete 'TLSConf' . You must sete 'tlsCert' and 'tlsKey' at a mininum.
nullTLSConf :: TLSConf
nullTLSConf =
    TLSConf { tlsPort      = 443
            , tlsCert      = ""
            , tlsKey       = ""
            , tlsCA        = Nothing
            , tlsTimeout   = 30
            , tlsLogAccess = Just logMAccess
            , tlsValidator = Nothing
            }


-- | record that holds the 'Socket' and 'SSLContext' needed to start
-- the https:\/\/ event loop. Used with 'simpleHTTPWithSocket''
--
-- see also: 'httpOnSocket'
data HTTPS = HTTPS
    { httpsSocket :: Socket
    , sslContext  :: ServerParams
    }

-- | generate the 'HTTPS' record needed to start the https:\/\/ event loop
--
httpsOnSocket :: FilePath  -- ^ path to ssl certificate
              -> FilePath  -- ^ path to ssl private key
              -> Maybe FilePath -- ^ path to PEM encoded list of CA certificates
              -> Socket    -- ^ listening socket (on which listen() has been called, but not accept())
              -> IO HTTPS
httpsOnSocket cert key _ socket =
    do creds <- credentialLoadX509 cert key
       let credentials = either (\msg -> error $ "Can't load certificate " ++ cert ++ " and key " ++ key ++ ": " ++ msg) id creds
       let params = def {
            serverSupported = def { supportedCiphers = ciphersuite_strong },
            serverShared = def {
                sharedCredentials = Credentials [credentials]
             }
         }
--       case mca of
--         Nothing   -> return ()
--         (Just ca) -> SSL.contextSetCAFile ctx ca

       return (HTTPS socket params)

-- | accept a TLS connection
acceptTLS :: Socket      -- ^ the socket returned from 'acceptLite'
          -> ServerParams
          -> IO Context
acceptTLS sck params =
      handle (\ (e :: SomeException) -> sClose sck >> throwIO e) $ do
          ssl <- contextNew sck params
          handshake ssl
          return ssl

-- | https:// 'Request'/'Response' loop
--
-- This function initializes SSL, and starts accepting and handling
-- 'Request's and sending 'Respone's.
--
-- Each 'Request' is processed in a separate thread.
listenTLS :: TLSConf                  -- ^ tls configuration
          -> (Request -> IO Response) -- ^ request handler
          -> IO ()
listenTLS tlsConf hand =
    do
       tlsSocket <- listenOn (tlsPort tlsConf)
       https     <- httpsOnSocket (tlsCert tlsConf) (tlsKey tlsConf) (tlsCA tlsConf) tlsSocket
       listenTLS' (tlsTimeout tlsConf) (tlsLogAccess tlsConf) https hand

-- | low-level https:// 'Request'/'Response' loop
--
-- This is the low-level loop that reads 'Request's and sends
-- 'Respone's. It assumes that SSL has already been initialized and
-- that socket is listening.
--
-- Each 'Request' is processed in a separate thread.
--
-- see also: 'listenTLS'
listenTLS' :: Int -> Maybe (LogAccess UTCTime) -> HTTPS -> (Request -> IO Response) -> IO ()
listenTLS' timeout mlog https@(HTTPS lsocket _) handler = do
#ifndef mingw32_HOST_OS
  installHandler openEndedPipe Ignore Nothing
#endif
  tm <- initialize (timeout * (10^(6 :: Int)))
  do let work :: (Socket, Context, HostName, PortNumber) -> IO ()
         work (socket, ssl, hn, p) =
             do -- add this thread to the timeout table
                tid     <- myThreadId
                thandle <- register tm $ do shutdownClose socket ssl
                                            killThread tid
                -- handle the request
                let timeoutIO = TSS.timeoutSocketIO thandle socket ssl

                request timeoutIO mlog (hn, fromIntegral p) handler
                            `E.catches` [ Handler ignoreConnectionAbruptlyTerminated
                                        , Handler ehs
                                        ]

                -- remove thread from timeout table
                cancel thandle

                -- close connection
                shutdownClose socket ssl

         loop :: IO ()
         loop = forever $ do -- do a normal accept
                             (sck, peer, port) <- acceptLite (httpsSocket https)
                             forkIO $ do -- do the TLS accept/handshake
                                         ssl <- acceptTLS sck (sslContext https)
                                         work (sck, ssl, peer, port)
                                           `catch` (\(e :: SomeException) -> do
                                                          shutdownClose sck ssl
                                                          throwIO e)
                             return ()
         pe e = log' ERROR ("ERROR in https accept thread: " ++ show e)
         infi = loop `catchSome` pe >> infi
     -- sockName <- getSocketName lsocket
     sockPort <- socketPort lsocket
     log' NOTICE ("Listening for https:// on port " ++ show sockPort)
     (infi `catch` (\e -> do log' ERROR ("https:// terminated by " ++ show (e :: SomeException))
                             throwIO e))
       `finally` (sClose lsocket)

         where
           shutdownClose :: Socket -> Context -> IO ()
           shutdownClose _ ssl =
               do bye ssl          `E.catch` ignoreException
                  contextClose ssl `E.catch` ignoreException

           -- exception handlers
           ignoreConnectionAbruptlyTerminated :: TLSException -> IO ()  -- FIXME
           ignoreConnectionAbruptlyTerminated _ = return ()

           ignoreSSLException :: TLSException -> IO ()
           ignoreSSLException _ = return ()

           ignoreException :: SomeException -> IO ()
           ignoreException _ = return ()

           ehs :: SomeException -> IO ()
           ehs x = when ((fromException x) /= Just ThreadKilled) $ log' ERROR ("HTTPS request failed with: " ++ show x)

           catchSome op h =
               op `E.catches` [ Handler $ ignoreSSLException
                              , Handler $ \(e :: ArithException) -> h (toException e)
                              , Handler $ \(e :: ArrayException) -> h (toException e)
                              , Handler $ \(e :: IOException)    ->
                                  if isFullError e || isDoesNotExistError e || isResourceVanishedError e
                                  then return () -- h (toException e) -- we could log the exception, but there could be thousands of them
                                  else log' ERROR ("HTTPS accept loop ignoring " ++ show e)
                              ]
           isResourceVanishedError :: IOException -> Bool
           isResourceVanishedError = isResourceVanishedType . ioeGetErrorType
           isResourceVanishedType :: IOErrorType -> Bool
           isResourceVanishedType ResourceVanished = True
           isResourceVanishedType _                = False
