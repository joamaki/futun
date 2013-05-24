module Main where

import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Network.Socket hiding (recv, recvFrom)
import Network.Socket.ByteString (recv, recvFrom, sendAll, sendAllTo)
import Prelude hiding (catch)
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Posix.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe

import qualified TunTap as TunTap

-- futun: A simple IP-over-UDP tunneling using TUN/TAP
-- usage:
--   futun server port
--   futun client host port

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO $ io
          `catch` handler
          `finally` putMVar mvar ()
  where
    handler (SomeException e) = putStrLn $ "thread exited with exception: " ++ show e

makeUDPSocket :: ServiceName -> IO (Socket, AddrInfo)
makeUDPSocket port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE] } -- listen on all
  ais <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily (head ais)) Datagram defaultProtocol
  bindSocket sock (addrAddress (head ais))
  return $ (sock, head ais)

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ "usage: "
  putStrLn $ "\t" ++ prog ++ " server [interface] [port]"
  putStrLn $ "\t" ++ prog ++ " client [interface] [hostname] [port]"
  exitSuccess

openTunHandle :: String -> IO (Maybe Handle)
openTunHandle name = do
  mfd <- TunTap.openTun name
  case mfd of 
    Nothing -> 
      return Nothing
    Just fd -> do
      h <- fdToHandle fd
      hSetBuffering h NoBuffering
      return $ Just h

server :: Handle -> ServiceName -> IO ()
server tunh port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE] } -- listen on all
  ais <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily (head ais)) Datagram defaultProtocol
  bindSocket sock (addrAddress (head ais))

  putStrLn $ "Waiting for hello on port " ++ (show port) ++ "... "
  (hello, addr) <- recvFrom sock 1024
  when ((B8.unpack hello) /= "hello") $ error "Received unknown hello!"
  
  _ <- forkChild . forever $ do
    buf <- recv sock 8192
    B.hPut tunh buf

  _ <- forkChild . forever $ do
    buf <- B.hGetSome tunh 8192
    sendAllTo sock buf addr

  putStrLn $ "Tunnel opened to " ++ (show addr)

client :: Handle -> HostName -> ServiceName -> IO ()
client tunh hostname port = do
  ais <- getAddrInfo Nothing (Just hostname) (Just port)
  let saddr = addrAddress $ head ais
  sock <- socket (addrFamily (head ais)) Datagram defaultProtocol
  connect sock saddr
  sendAll sock (B8.pack "hello")

  _ <- forkChild . forever $ do
    buf <- recv sock 8192
    B.hPut tunh buf

  _ <- forkChild . forever $ do
    buf <- B.hGetSome tunh 8192
    sendAll sock buf

  putStrLn $ "Tunnel opened to " ++ (show saddr)

main :: IO ()
main = do
  argv <- getArgs
  when (length argv < 2) $ usage
  tunh <- openTunHandle (argv !! 1)
  when (isNothing tunh) $ do
    putStrLn $ "Could not open tunnel interface " ++ (argv !! 1) ++ "!"
    exitSuccess
   
  case (argv !! 0) of
    "server" -> server (fromJust tunh) (argv !! 2)
    "client"-> client (fromJust tunh) (argv !! 2) (argv !! 3)
    _ -> usage

  waitForChildren
