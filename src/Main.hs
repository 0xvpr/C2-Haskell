{--
 - Creator: VPR
 - Created: November 28, 2021
 - Updated: November 29, 2021
 -
 - Description:
 -     A Command & Control interface that establishes a client/server model
 -     that uses TCP sockets.
--}

module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (unless, forever, void)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception as E
import Data.ByteString as B
import Data.Binary
import Data.Maybe

main :: IO ()
main = runTCPServer Nothing "9001" mainLoop

mainLoop :: Socket -> IO ()
mainLoop clientSock = do
    recv_buffer <- recv clientSock 4096
    print recv_buffer
    unless (B.null recv_buffer) $ do
        sendAll clientSock recv_buffer
        mainLoop clientSock

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO ()) -> IO ()
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop

    where
    resolve = do
        let hints = defaultHints {
            addrFlags = [AI_PASSIVE],
            addrSocketType = Stream
        }
        Prelude.head <$> getAddrInfo (Just hints) mhost (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        
        bind sock $ addrAddress addr
        listen sock 4096
        return sock

    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
