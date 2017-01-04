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

import DMSS.Daemon.Command
import DMSS.Config

import Data.Default (def, Default)
import System.Daemon
import Control.Pipe.C3 ( commandReceiver )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad (forever)
import System.IO.Silently (silence)

type Response = String

checkerDaemon :: Command -> IO Response
checkerDaemon Status  = return "Daemon is running!"
checkerDaemon Version = return $ "Daemon version: " ++ daemonVersion

data Options = Options
  { daemonSilent :: Bool }

instance Default Options where
  def = Options False

defaultOptions :: Options
defaultOptions = (def :: Options)

daemonMain :: Options -> IO ()
daemonMain o = do
  -- Start event loop
  logMsg o "Starting event loop..."
  _ <- forkIO $ forever $ do
    let ms = 10000
        num_ms = 1000
    threadDelay (ms * num_ms)
    logMsgLn o $ "heartbeat"
  logMsgLn o "Done"

  -- Make sure local directory exists for storing data
  createLocalDirectory

  -- Start daemon process
  runInForeground 5000 (commandReceiver checkerDaemon)

-- Log messages functions. Simply outputs to stdout for now.
logMsg :: Options -> String -> IO ()
logMsg o s = silenceIf o $ putStr s
logMsgLn :: Options -> String -> IO ()
logMsgLn o s = silenceIf o $ putStrLn s
silenceIf :: Options -> IO a -> IO a
silenceIf o p = if daemonSilent o then silence p else p

daemonVersion :: String
daemonVersion = "0.1.0"
