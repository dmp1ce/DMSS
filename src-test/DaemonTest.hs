module DaemonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck
--
import Control.Concurrent

import DMSS.Daemon  ( daemonMain, defaultOptions, daemonSilent )
import DMSS.CLI     ( runCommand )
import DMSS.Command ( Command (Status) )

tests :: [TestTree]
tests =
  [ testCase "Daemon Startup" daemonStartUp ]

daemonStartUp :: Assertion
daemonStartUp = do
  -- Start daemon silently
  t <- forkIO $ daemonMain (defaultOptions { daemonSilent = True } )
  -- Allow Daemon to start. 1 second delay
  threadDelay (1000 * 1000 * 1)
  -- Verify it is running with status command
  r <- runCommand Status

  -- Stop Daemon
  killThread t

  Just "Daemon is running!" @=? r
