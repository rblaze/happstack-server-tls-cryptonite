{-# LANGUAGE CPP, ScopedTypeVariables #-}
{- | core functions and types for HTTPS support
-}
module Happstack.Server.Internal.TLS where

import Control.Concurrent                         (forkIO, killThread, myThreadId)
import Control.Exception                          (finally)
import Control.Exception.Extensible               as E
import Control.Monad                              (forever, when)
import Data.Time                                  (UTCTime)
import Happstack.Server.Internal.Listen           (listenOn)
import Happstack.Server.Internal.Handler          (request)
import Happstack.Server.Internal.Socket           (acceptLite)
import Happstack.Server.Internal.TimeoutManager   (cancel, initialize, register)
import Happstack.Server.Internal.TimeoutSocketTLS as TSS
import Happstack.Server.Internal.Types            (Request, Response)
import Network.Socket                             (HostName, PortNumber, Socket, getSocketName, sClose, socketPort)
import           OpenSSL                          (withOpenSSL)
import           OpenSSL.Session                  (SSL, SSLContext)
import qualified OpenSSL.Session                  as SSL
import Happstack.Server.Internal.TimeoutIO        (TimeoutIO(toHandle, toShutdown))
import Happstack.Server.Types                     (LogAccess, logMAccess)
import System.IO.Error                            (isFullError)
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
    , sslContext  :: SSLContext
    }

-- | generate the 'HTTPS' record needed to start the https:\/\/ event loop
--
httpsOnSocket :: FilePath  -- ^ path to ssl certificate
              -> FilePath  -- ^ path to ssl private key
              -> Socket    -- ^ listening socket (on which listen() has been called, but not accept())
              -> IO HTTPS
httpsOnSocket cert key socket =
    do ctx <- SSL.context
       SSL.contextSetPrivateKeyFile  ctx key
       SSL.contextSetCertificateFile ctx cert
       SSL.contextSetDefaultCiphers  ctx

       b <- SSL.contextCheckPrivateKey ctx
       when (not b) $ error $ "OpenTLS certificate and key do not match."

       return (HTTPS socket ctx)

-- | accept a TLS connection
acceptTLS :: HTTPS -> IO (SSL, HostName, PortNumber)
acceptTLS (HTTPS sck' ctx) =
    do -- do normal accept
      (sck, peer, port) <- acceptLite sck'

      --  then TLS accept
      ssl <- SSL.connection ctx sck
      SSL.accept ssl

      return (ssl, peer, port)

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
    do withOpenSSL $ return ()
       tlsSocket <- listenOn (tlsPort tlsConf)
       https     <- httpsOnSocket (tlsCert tlsConf) (tlsKey tlsConf) tlsSocket
       listenTLS' (tlsTimeout tlsConf) (tlsLogAccess tlsConf) tlsSocket https hand

-- | low-level https:// 'Request'/'Response' loop
--
-- This is the low-level loop that reads 'Request's and sends
-- 'Respone's. It assumes that SSL has already been initialized and
-- that socket is listening.
--
-- Each 'Request' is processed in a separate thread.
--
-- see also: 'listenTLS'
listenTLS' :: Int -> Maybe (LogAccess UTCTime) -> Socket -> HTTPS -> (Request -> IO Response) -> IO ()
listenTLS' timeout mlog socket https hand = do
#ifndef mingw32_HOST_OS
  installHandler openEndedPipe Ignore Nothing
#endif
  tm <- initialize (timeout * (10^(6 :: Int)))
  do let work :: (SSL, HostName, PortNumber) -> IO ()
         work (ssl, hn, p) = 
             do tid     <- myThreadId
                thandle <- register tm $ do SSL.shutdown ssl SSL.Unidirectional `E.catch` ignoreSSLException
                                            killThread tid
                let timeoutIO = TSS.timeoutSocketIO thandle ssl
                request timeoutIO mlog (hn,fromIntegral p) hand `E.catches` [ Handler ignoreConnectionAbruptlyTerminated
                                                                            , Handler ehs 
                                                                            ]
                -- remove thread from timeout table
                cancel (toHandle timeoutIO)
                toShutdown timeoutIO `E.catch` ignoreSSLException

         loop :: IO ()
         loop = forever $ do w <- acceptTLS https
                             forkIO $ work w
                             return ()
         pe e = log' ERROR ("ERROR in https accept thread: " ++ show e)
         infi = loop `catchSome` pe >> infi
     sockName <- getSocketName socket
     sockPort <- socketPort socket
     log' NOTICE ("Listening on https://" ++ show sockName ++":" ++ show sockPort)
     infi `finally` (sClose socket)
         where
           ignoreSSLException :: SSL.SomeSSLException -> IO ()
           ignoreSSLException _ = return ()
           -- exception handlers
           ignoreConnectionAbruptlyTerminated :: SSL.ConnectionAbruptlyTerminated -> IO ()
           ignoreConnectionAbruptlyTerminated _ = return ()

           ehs :: SomeException -> IO ()
           ehs x = when ((fromException x) /= Just ThreadKilled) $ log' ERROR ("HTTPS request failed with: " ++ show x)

           catchSome op h = 
               op `E.catches` [ Handler ignoreConnectionAbruptlyTerminated
                              , Handler $ \(e :: ArithException) -> h (toException e)
                              , Handler $ \(e :: ArrayException) -> h (toException e)
                              , Handler $ \(e :: IOException)    ->
                                  if isFullError e
                                  then return () -- h (toException e) -- we could log the exception, but there could be thousands of them
                                  else throw e
                              ]
