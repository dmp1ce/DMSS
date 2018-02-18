-- |
-- Module      : DMSS.Daemon
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System daemon module
--

{-# LANGUAGE OverloadedStrings #-}

module DMSS.Daemon where

import DMSS.Config
import DMSS.Daemon.Command
import DMSS.Daemon.CLI ( Cli, Options, Options (daemonSilent), daemonMain )
import DMSS.Daemon.Common ( cliPort, peerPort )
import DMSS.Storage ( StorageT, runStoragePool
                    , latestCheckIns, verifyPublicCheckIn
                    , unName
                    )
import Paths_DMSS ( version )

import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad (forever, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Pipe.C3 ( commandReceiver )
import Data.Version ( showVersion )
import Data.Foldable ( traverse_ )
import Data.Default (def)
import System.Daemon
import System.IO.Silently (silence)
import Network.Socket
  ( socket, Socket, defaultProtocol, bind, iNADDR_ANY, listen, close
  , Family (AF_INET), SocketType (Stream), SockAddr (SockAddrInet), accept )

type Response = String

checkerDaemon :: Command -> IO Response
checkerDaemon Status  = return "Daemon is running!"
checkerDaemon Version = return $ "Daemon version: " ++ showVersion version

eventLoop :: Options -> StorageT ()
eventLoop o = do
  -- Check checkin status of all users
  userCheckIns <- latestCheckIns
  -- Valid checkin if any valid checkin with latestCheckIns timeframe
  checkInsValid <- traverse (\(n,ps) ->
    (,) <$> pure n
        <*> foldM (\a p ->
            if a
            then pure a
            else verifyPublicCheckIn n p) False ps
    ) userCheckIns

  liftIO $ traverse_
            (\(n,v) ->
              if v
              then logMsgLn o $ unName n ++ " has a valid checkin."
              else logMsgLn o $ unName n ++ " has not checked in recently!"
            ) checkInsValid

peerLoop :: Socket -> IO ()
peerLoop sock = do
  conn <- accept sock
  -- TODO: Figure out what to do with this connection
  putStrLn $ "Recieved a connection (" ++ show conn ++ ") ."
  close sock
  peerLoop sock

daemonMain :: IO ()
daemonMain = DMSS.Daemon.CLI.daemonMain (process def)

process :: Options -> Cli -> IO ()
process o _ = do
  -- Make sure local directory exists for storing data
  createLocalDirectory

  -- Start event loop
  logMsgLn o "Starting event loop"
  -- Start daemon process
  _ <- forkIO $ forever $ do
    let ms = 10000
        num_ms = 1000
    threadDelay (ms * num_ms)
    runStoragePool $ eventLoop o

  logMsgLn o $ "Listening for peers on port " ++ show peerPort
  sock <- socket AF_INET Stream defaultProtocol
  --setSockOptions sock ReuseAddr 1
  bind sock (SockAddrInet peerPort iNADDR_ANY)
  listen sock 2
  _ <- forkIO $ peerLoop sock

  logMsgLn o $ "Listening for CLI commands on port " ++ show cliPort
  logMsgLn o "== CTRL-C to quit =="
  runInForeground cliPort (commandReceiver checkerDaemon)

-- Log messages functions. Simply outputs to stdout for now.
logMsg :: Options -> String -> IO ()
logMsg o s = silenceIf o $ putStr s
logMsgLn :: Options -> String -> IO ()
logMsgLn o s = silenceIf o $ putStrLn s
silenceIf :: Options -> IO a -> IO a
silenceIf o p = if daemonSilent o then silence p else p
