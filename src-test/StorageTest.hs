module StorageTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import DMSS.Storage ( getUserKeyKey
                    , migrateStorage
                    , storeUserKey
                    , Fingerprint (..)
                    )

import Common

tests :: [TestTree]
tests =
  [ testCase "storeUserKeyTest" storeUserKeyTest
  , testCase "storeCheckInTest" storeCheckInTest
  , testCase "removeUserKeyTest" removeUserKeyTest
  ]

tempDir :: FilePath
tempDir = "storageTest"

storeUserKeyTest :: Assertion
storeUserKeyTest = withTemporaryTestDirectory tempDir ( \_ -> do
    -- Prepare database
    _ <- migrateStorage

    -- Store fake user key
    let fpr = Fingerprint "hello1234"
    _ <- storeUserKey fpr

    -- Check that the fake user key was stored
    k <- getUserKeyKey fpr
    case k of
      Nothing -> assertFailure "Could not find UserKey based on (Fingerprint)"
      _       -> return ()
  )

storeCheckInTest :: Assertion
storeCheckInTest = assertFailure "No test here!"

removeUserKeyTest :: Assertion
removeUserKeyTest = assertFailure "No test here!"
