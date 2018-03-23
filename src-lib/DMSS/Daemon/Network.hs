-- |
-- Module      : DMSS.Daemon
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System daemon network functions
--

module DMSS.Daemon.Network where

import DMSS.Daemon.Memory (DaemonTVar)
import DMSS.Storage.TH (Peer (Peer))
import DMSS.Storage (Host (Host), Port (Port))
import Network.Socket
  ( close, accept, withSocketsDo, defaultHints, addrSocketType, getAddrInfo
  , addrFamily, addrSocketType, addrProtocol, addrAddress, SocketType (Stream), socket
  , connect, bind, PortNumber, Family (AF_INET), Socket, HostAddress, inet_ntoa
  , listen, addrFlags, AddrInfoFlag (AI_PASSIVE), setSocketOption, SocketOption (ReuseAddr)
  , inet_addr, isConnected )
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Char8 as C
import Control.Monad (forever)
import Control.Concurrent (forkFinally, ThreadId, threadDelay)
import Control.Concurrent.STM.TVar ( modifyTVar )
import Control.Monad.STM ( atomically )

-- | Listing for messages from connection
connListen :: Socket -> DaemonTVar -> IO ()
connListen sock sm = do
  putStrLn $ "Listening for messages on connection " ++ show sock
  msg <- recv sock 1024
  if msg == ""
  then putStrLn $ "Connection " ++ show sock ++ " closed remotely."
  else do putStr "Received: "
          C.putStrLn msg
          connListen sock sm

-- | Add connection to shared memory
addConn :: Socket -> DaemonTVar -> IO ThreadId
addConn sock sm = do
  atomically $ modifyTVar sm ((:) sock)
  t <- forkFinally (connListen sock sm) (\_ -> close sock)
  putStrLn $ "Trying to send out a hello message to on connection " ++ show sock
  threadDelay $ 1000 * 1000
  sendAll sock "Hello, peer!"
  return t

-- | Attempt to create a connection with a listening peer
connAttempt :: (HostAddress, PortNumber) -> DaemonTVar -> IO (Maybe ThreadId)
connAttempt (h,p) sm = withSocketsDo $ do
  addr <- resolve h p
  print addr
  sock <- open addr
  connected <- isConnected sock
  if connected
  then Just <$> addConn sock sm
  else close sock >> return Nothing
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream
                               , addrFamily = AF_INET }
      host' <- inet_ntoa host
      addr:_ <- getAddrInfo (Just hints) (Just host') (Just $ show port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

-- | Listen for incoming connections
incomingConnListen :: DaemonTVar -> PortNumber -> IO ()
incomingConnListen sm port = withSocketsDo $ do
  addr <- resolve port
  sock <- open addr
  loop sock
  where
    resolve port' = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE]
                               , addrSocketType = Stream
                               , addrFamily = AF_INET
                               }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show port')
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock
    loop sock = forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Connection from " ++ show peer
      addConn conn sm

-- | Use DNS to lookup Peer's `HostAddress` and also return `PortNumber`
lookupPeerHostPort :: Peer -> IO (HostAddress,PortNumber)
lookupPeerHostPort ( Peer (Host h) (Port p) _) = do
  -- TODO: Determine if the Peer Hostname needs to be looked up
  -- TODO: Lookup HostAddress
  h' <- inet_addr h
  return (h', p)
