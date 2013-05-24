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
  putStrLn $ "\t" ++ prog ++ " server [port]"
  putStrLn $ "\t" ++ prog ++ " client [hostname] [port]"
  exitSuccess

openTunHandle :: IO (String, Handle)
openTunHandle = do
  (tunif, tunfd) <- TunTap.openTun
  h <- fdToHandle tunfd
  hSetBuffering h NoBuffering
  return $ (tunif, h)

server :: ServiceName -> IO ()
server port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE] } -- listen on all
  ais <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily (head ais)) Datagram defaultProtocol
  bindSocket sock (addrAddress (head ais))

  putStrLn $ "Waiting for hello on port " ++ (show port) ++ "... "
  (hello, addr) <- recvFrom sock 1024
  when ((B8.unpack hello) /= "hello") $ error "Received unknown hello!"
  
  (iface, tunh) <- openTunHandle

  _ <- forkChild . forever $ do
    buf <- recv sock 8192
    B.hPut tunh buf

  _ <- forkChild . forever $ do
    buf <- B.hGetSome tunh 8192
    sendAllTo sock buf addr

  putStrLn $ "Tunnel " ++ iface ++ " opened to " ++ (show addr)

client :: HostName -> ServiceName -> IO ()
client hostname port = do
  ais <- getAddrInfo Nothing (Just hostname) (Just port)
  let saddr = addrAddress $ head ais
  sock <- socket (addrFamily (head ais)) Datagram defaultProtocol
  connect sock saddr
  sendAll sock (B8.pack "hello")

  (iface, tunh) <- openTunHandle

  _ <- forkChild . forever $ do
    buf <- recv sock 8192
    B.hPut tunh buf

  _ <- forkChild . forever $ do
    buf <- B.hGetSome tunh 8192
    sendAll sock buf

  putStrLn $ "Tunnel " ++ iface ++ " opened to " ++ (show saddr)

main :: IO ()
main = do
  argv <- getArgs
  when (length argv < 1) $ usage
  case (argv !! 0) of
    "server" -> server (argv !! 1)
    "client"-> client (argv !! 1) (argv !! 2)
    _ -> usage

  waitForChildren
