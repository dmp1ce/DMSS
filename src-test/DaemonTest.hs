module DaemonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent

import DMSS.Config     ( localDirectory )
import DMSS.Daemon     ( daemonMain )
import DMSS.CLI        ( runCommand )
import DMSS.Daemon.Command ( Command (Status) )
import Common ( withTemporaryTestDirectory )
import System.Environment (withArgs)
import Data.List (isPrefixOf)

tests :: [TestTree]
tests =
  [ testCase "Daemon Startup" daemonStartUp ]

tempDir :: FilePath
tempDir = "daemonTest"

daemonStartUp :: Assertion
daemonStartUp = withTemporaryTestDirectory tempDir ( \homedir -> do
    -- Start daemon silently
    t <- forkIO $ withArgs ["-s"] daemonMain
    -- Allow Daemon to start. 1 second delay
    threadDelay (1000 * 1000)
    -- Verify it is running with status command
    r <- runCommand Status

    -- Stop Daemon
    killThread t

    -- Make sure home directory is correct
    l <- localDirectory
    assertBool
      (homedir ++ " is not the prefix of actual home directory " ++ l)
      (isPrefixOf homedir l)

    Just "Daemon is running!" @=? r
  )
