module DaemonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent

import DMSS.Config     ( localDirectory )
import DMSS.Daemon     ( daemonMain )
import DMSS.CLI        ( runCommand, cliMain )
import DMSS.Daemon.Command ( Command (Status) )
import Common ( withTemporaryTestDirectory )
import System.Environment (withArgs)
import Data.List (isPrefixOf)
import DMSS.Daemon.Common ( cliPort )

tests :: [TestTree]
tests =
  [ testCase "Daemon startup" daemonStartUp
  , testCase "Two daemon startup" twoDaemonStartUp]

tempDir :: FilePath
tempDir = "daemonTest"

-- NOTICE: Tests are run with different ports to prevent threads cleanup
-- from effecting ports on another test.
--
-- Using the same port from test to test will cause tests to not be able to
-- connect.

daemonStartUp :: Assertion
daemonStartUp = withTemporaryTestDirectory tempDir ( \homedir -> do
    -- Start daemon silently
    t <- forkIO $ withArgs ["-s"] daemonMain
    -- Allow Daemon to start. 1 second delay
    threadDelay (1000 * 1000)
    -- Verify it is running with status command
    r <- runCommand cliPort Status

    -- Stop Daemon
    killThread t

    -- Make sure home directory is correct
    l <- localDirectory
    assertBool
      (homedir ++ " is not the prefix of actual home directory " ++ l)
      (homedir `isPrefixOf` l)

    Just "Daemon is running!" @=? r
  )

-- Test that Daemons can be run side by side without colliding in any way
twoDaemonStartUp :: Assertion
twoDaemonStartUp = withTemporaryTestDirectory tempDir $ \h1-> do
  -- Start daemon silently
  t1 <- forkIO $ withArgs
                  [ "--homedir=" ++ h1
                  , "--cli-port=7006"
                  , "--peer-port=7007"
                  , "-s"
                  ] daemonMain
  -- Allow Daemon to start. 1 second delay
  threadDelay (1000 * 1000)

  -- Make sure home directory is correct
  l1 <- localDirectory
  assertBool
    (h1 ++ " is not the prefix of actual home directory " ++ l1)
    (h1 `isPrefixOf` l1)

  -- Start daemon two silently with different home directory
  let h2 = h1 ++ "daemon2"
  t2 <- forkIO $ withArgs
                  [ "--homedir=" ++ h2
                  , "--cli-port=6006"
                  , "--peer-port=6007"
                  , "-s"
                  ] daemonMain
  -- Allow Daemon to start. 1 second delay
  threadDelay (1000 * 1000)

  -- Verify both daemons are running with status command
  --r1 <- withArgs [] (runCommand cliPort Status)
  _ <- withArgs ["--silent", "--homedir=" ++ h1, "--port=7006", "status"] cliMain
  killThread t1

  threadDelay (1000 * 1000)
  _ <- withArgs ["--silent", "--homedir=" ++ h2, "--port=6006", "status"] cliMain
  killThread t2

  -- Make sure home directory is correct
  l2 <- localDirectory
  assertBool
    (h2 ++ " is not the prefix of actual home directory " ++ l1)
    (h2 `isPrefixOf` l2)
