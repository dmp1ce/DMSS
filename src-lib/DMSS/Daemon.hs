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

module DMSS.Daemon where

import DMSS.Config (createLocalDirectory)
import DMSS.Daemon.Command
import DMSS.Daemon.Memory (DaemonTVar, isPeerConnected, cleanupConnections)
import DMSS.Daemon.Network (connAttempt, incomingConnListen, lookupPeerHostPort)
import DMSS.Daemon.CLI ( Cli (Cli), daemonMain, FlagSilent (SilentOn) )
import DMSS.Storage ( StorageT, runStoragePool
                    , latestCheckIns, verifyPublicCheckIn
                    , unName, listPeers
                    , dbConnectionString
                    )
import DMSS.Storage.TH ( migrateAll )
import Paths_DMSS ( version )

import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad (forever, foldM, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Pipe.C3 ( commandReceiver )
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Concurrent.STM.TVar (newTVar, readTVar)
import Control.Monad.STM (atomically)
import Data.Version ( showVersion )
import Data.Foldable ( traverse_ )
import Data.Text ( pack, unpack )
import System.Daemon
import System.IO.Silently (silence)
import System.Environment (setEnv)
import qualified Database.Persist.Sqlite as P
import qualified Control.Exception as E

type Response = String

checkerDaemon :: Command -> IO Response
checkerDaemon Status  = return "Daemon is running!"
checkerDaemon Version = return $ "Daemon version: " ++ showVersion version

eventLoop :: Cli -> DaemonTVar -> StorageT ()
eventLoop (Cli _ _ _ s) sm = do
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
              then logMsgLn s $ unName n ++ " has a valid checkin."
              else logMsgLn s $ unName n ++ " has not checked in recently!"
            ) checkInsValid

  -- TODO: Try to connect to all peers which don't currently have connections
  --       Create shared variable for holding active connections.
  currMem <- liftIO $ atomically $ readTVar sm
  liftIO $ do putStrLn "Global memory dump:"; print currMem
  -- TODO: Exponentially backoff peers that are not responding
  -- TODO: Get resolved HostAddress and PortNumber in order to determine what connections to attempt
  peers <- listPeers
  peers' <- liftIO $ traverse (lookupPeerHostPort . snd) peers

  liftIO $ cleanupConnections sm
  liftIO $ traverse_ (\peer -> do
                         alreadyConnected <- isPeerConnected sm peer
                         unless alreadyConnected (void $ connAttempt peer sm)) peers'


daemonMain :: IO ()
daemonMain = DMSS.Daemon.CLI.daemonMain process

process :: Cli -> IO ()
process cli@(Cli h cp pp s) = do
  -- Make sure local directory exists for storing data
  mapM_ (setEnv "HOME") h
  createLocalDirectory

  -- Create shared memory for all threads
  sm <- atomically $ newTVar []

  -- Create shared storage pool for all processes
  c <- dbConnectionString
  runStdoutLoggingT $ P.withSqlitePool (pack c) 10 $ \pool -> do
    -- Run migrations
    liftIO $ runStoragePool pool $ P.runMigrationSilent migrateAll >>= liftIO . mapM_ (putStrLn . unpack)

    liftIO $ logMsgLn s "Starting event loop"
    _ <- liftIO $ forkIO $ forever $ do
      let ms = 10000
          num_ms = 1000
      threadDelay (ms * num_ms)
      -- TODO: Try to be better about catching specific errors that could occur
      err <- E.try $ runStoragePool pool $ eventLoop cli sm :: IO (Either E.SomeException ())
      either print return err

    liftIO $ logMsgLn s $ "Listening for peers on port " ++ show pp
    _ <- liftIO $ forkIO $ do
      -- TODO: Save listening thread if needed
      _ <- incomingConnListen sm pp
      return ()
    return ()

  logMsgLn s $ "Listening for CLI commands on port " ++ show cp
  logMsgLn s "== CTRL-C to quit =="
  runInForeground (fromIntegral cp) (commandReceiver checkerDaemon)

-- Log messages functions. Simply outputs to stdout for now.
logMsg :: FlagSilent -> String -> IO ()
logMsg s str = silenceIf s $ putStr str
logMsgLn :: FlagSilent -> String -> IO ()
logMsgLn s str = silenceIf s $ putStrLn str
silenceIf :: FlagSilent -> IO a -> IO a
silenceIf s p = if s == SilentOn then silence p else p
