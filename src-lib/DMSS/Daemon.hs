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
import DMSS.Storage ( StorageT, runStoragePool
                    , latestCheckIns, verifyPublicCheckIn
                    , unName
                    )
import Paths_DMSS ( version )

import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad (forever, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Pipe.C3 ( commandReceiver )
import Data.Default (def, Default)
import Data.Version ( showVersion )
import Data.Foldable ( traverse_ )
import System.Daemon
import System.IO.Silently (silence)


type Response = String

checkerDaemon :: Command -> IO Response
checkerDaemon Status  = return "Daemon is running!"
checkerDaemon Version = return $ "Daemon version: " ++ showVersion version

newtype Options = Options { daemonSilent :: Bool }

instance Default Options where
  def = Options False

defaultOptions :: Options
defaultOptions = def :: Options

mainLoop :: Options -> StorageT ()
mainLoop o = do
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

daemonMain :: Options -> IO ()
daemonMain o = do
  -- Make sure local directory exists for storing data
  createLocalDirectory

  -- Start event loop
  logMsg o "Starting event loop..."
  -- Start daemon process
  _ <- forkIO $ forever $ do
    let ms = 10000
        num_ms = 1000
    threadDelay (ms * num_ms)
    runStoragePool $ mainLoop o
  logMsgLn o "Done"

  runInForeground 5000 (commandReceiver checkerDaemon)

-- Log messages functions. Simply outputs to stdout for now.
logMsg :: Options -> String -> IO ()
logMsg o s = silenceIf o $ putStr s
logMsgLn :: Options -> String -> IO ()
logMsgLn o s = silenceIf o $ putStrLn s
silenceIf :: Options -> IO a -> IO a
silenceIf o p = if daemonSilent o then silence p else p
