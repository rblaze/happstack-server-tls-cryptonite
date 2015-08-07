{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- |
-- borrowed from snap-server. Check there periodically for updates.
-}
module Happstack.Server.Internal.Cryptonite.TimeoutSocketTLS where

import           Control.Exception             (SomeException, catch)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import qualified Happstack.Server.Internal.TimeoutManager as TM
import           Happstack.Server.Internal.TimeoutIO (TimeoutIO(..))
import           Network.Socket                (Socket)
import           Network.Socket.SendFile (ByteCount, Offset)
import           Network.TLS
import           System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek, withBinaryFile)
import           System.IO.Unsafe (unsafeInterleaveIO)

sPutLazyTickle :: TM.Handle -> Context -> L.ByteString -> IO ()
sPutLazyTickle thandle ssl cs =
    do L.foldrChunks (\c rest -> sendData ssl (L.fromStrict c) >> TM.tickle thandle >> rest) (return ()) cs
{-# INLINE sPutLazyTickle #-}

sPutTickle :: TM.Handle -> Context -> B.ByteString -> IO ()
sPutTickle thandle ssl cs =
    do sendData ssl (L.fromStrict cs)
       TM.tickle thandle
{-# INLINE sPutTickle #-}

sGetContents :: TM.Handle
             -> Context          -- ^ Connected socket
             -> IO L.ByteString  -- ^ Data received
sGetContents handle ssl =
    fmap L.fromChunks loop
    where
      loop = unsafeInterleaveIO $ do
               s <- recvData ssl
               TM.tickle handle
               if S.null s
                then do return []
                else do ss <- loop
                        return (s:ss)

timeoutSocketIO :: TM.Handle -> Socket -> Context -> TimeoutIO
timeoutSocketIO handle _ ssl =
    TimeoutIO { toHandle      = handle
              , toShutdown    = do bye ssl          `catch` ignoreException
                                   contextClose ssl `catch` ignoreException
              , toPutLazy     = sPutLazyTickle handle ssl
              , toPut         = sPutTickle     handle ssl
              , toGetContents = sGetContents   handle ssl
              , toSendFile    = sendFileTickle handle ssl
              , toSecure      = True
              }
    where
      ignoreException :: SomeException -> IO ()
      ignoreException _ = return ()

sendFileTickle :: TM.Handle -> Context -> FilePath -> Offset -> ByteCount -> IO ()
sendFileTickle thandle ssl fp offset count =
    do withBinaryFile fp ReadMode $ \h -> do
         hSeek h AbsoluteSeek offset
         c <- L.hGetContents h
         sPutLazyTickle thandle ssl (L.take (fromIntegral count) c)
