module ConfigTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import System.Unix.Directory
import System.Directory
import System.Environment

import DMSS.Config

tests :: [TestTree]
tests =
  [ testCase "Create config directory" createConfigDirectory
  ]

tempDir :: FilePath
tempDir = "configTest"

createConfigDirectory :: Assertion
createConfigDirectory = do
  withTemporaryDirectory tempDir ( \s -> do
    -- Change HOME environment variable to temporary directory
    setEnv "HOME" s
    -- Create local home directory for DMSS
    createLocalDirectory
    -- Check that the GPG context directory exists
    g <- gpgContext
    gExists <- doesDirectoryExist g
    assertBool ("GPG context directory (" ++ g ++ ")does not exist") gExists
    )
