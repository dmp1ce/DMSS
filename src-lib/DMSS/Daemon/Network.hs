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
{-# LANGUAGE OverloadedStrings #-}
module DMSS.Daemon.Network where

import DMSS.Daemon.Memory (DaemonTVar)
import DMSS.Storage (Port (Port), Host (Host))
import Network.Socket
  ( close, accept, withSocketsDo, defaultHints, addrSocketType, getAddrInfo
  , addrFamily, addrSocketType, addrProtocol, addrAddress, SocketType (Stream), socket
  , connect, bind, PortNumber, Family (AF_INET)
  , listen, addrFlags, AddrInfoFlag (AI_PASSIVE), setSocketOption, SocketOption (ReuseAddr) )
import Network.Socket.ByteString (sendAll, recv)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import Control.Monad (forever, void, unless)
import Control.Concurrent (forkFinally)

connAttempt :: Host -> DMSS.Storage.Port -> DaemonTVar -> IO ()
connAttempt (Host h) (Port p) _ = withSocketsDo $ do
  addr <- resolve h p
  print addr
  E.bracket (open addr) close talk
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream
                               , addrFamily = AF_INET }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = do
      -- TODO: Figure out what the connecting peer should do with this connection.
      sendAll sock "Hello, world!"
      msg <- recv sock 1024
      putStr "Received: "
      C.putStrLn msg

beginConnListen :: DaemonTVar -> PortNumber -> IO ()
beginConnListen _ port = withSocketsDo $ do
  addr <- resolve port
  _ <- E.bracket (open addr) close loop
  return ()
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
      void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
      -- TODO: Figure out what the listening peer should do with this connection.
      msg <- recv conn 1024
      unless (S.null msg) $ do
        sendAll conn msg
        talk conn
