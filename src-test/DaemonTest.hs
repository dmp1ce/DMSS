module DaemonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent

import DMSS.Daemon  ( daemonMain, defaultOptions, daemonSilent )
import DMSS.CLI     ( runCommand )
import DMSS.Daemon.Command ( Command (Status) )
import Common ( withTemporaryTestDirectory )

tests :: [TestTree]
tests =
  [ testCase "Daemon Startup" daemonStartUp ]

tempDir :: FilePath
tempDir = "daemonTest"

daemonStartUp :: Assertion
daemonStartUp = withTemporaryTestDirectory tempDir ( \_ -> do
    -- Start daemon silently
    t <- forkIO $ daemonMain (defaultOptions { daemonSilent = True } )
    -- Allow Daemon to start. 1 second delay
    threadDelay (1000 * 1000)
    -- Verify it is running with status command
    r <- runCommand Status

    -- Stop Daemon
    killThread t

    Just "Daemon is running!" @=? r
  )
