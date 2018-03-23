-- |
-- Module      : DMSS.Daemon
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System daemon memory
--

module DMSS.Daemon.Memory where

import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar)
import Control.Monad.STM (atomically)
import Network.Socket ( HostAddress, PortNumber, isConnected
                      , Socket, SockAddr (SockAddrInet), getPeerName
                      )

-- Manages concurrency between application threads
type DaemonTVar = TVar [Socket]

isPeerConnected :: DaemonTVar -> (HostAddress,PortNumber) -> IO Bool
isPeerConnected sm peer = do
  socks <- readTVarIO sm
  -- TODO: Determine if address is localhost or not
  --       `getSocketName` only return information about local sockets
  --       remote sockets need to use `getPeerName`
  -- Convert socks to a host,peer list
  foldr (\s a -> do
           ( SockAddrInet p h ) <- getPeerName s
           if (h,p) == peer then return True else a
       ) (return False) socks

-- Remove closed connections
cleanupConnections :: DaemonTVar -> IO ()
cleanupConnections sm = do
  socks <- readTVarIO sm
  socks' <- cleanup socks
  atomically $ writeTVar sm socks'
  where 
    cleanup :: [Socket] -> IO [Socket]
    cleanup [] = return []
    cleanup (s:ss) = do
      connected <- isConnected s
      if connected
      then do ss' <- cleanup ss
              return (s:ss')
      else cleanup ss
