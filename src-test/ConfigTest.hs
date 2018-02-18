module ConfigTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import System.Directory

import DMSS.Config

import Common

tests :: [TestTree]
tests =
  [ testCase "create_config_directory" createConfigDirectory
  ]

tempDir :: FilePath
tempDir = "configTest"

createConfigDirectory :: Assertion
createConfigDirectory = withTemporaryTestDirectory tempDir
  ( \_ -> do
    l <- localDirectory
    lExists <- doesDirectoryExist l
    assertBool ("Local directory (" ++ l ++ ") does not exist") lExists
  )
