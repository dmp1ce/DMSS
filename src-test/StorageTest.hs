module StorageTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import DMSS.Storage ( getUserKeyKey
                    , storeUserKey
                    , Fingerprint (..)
                    , CheckInProof (..)
                    , storeCheckIn
                    , listCheckIns
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
storeUserKeyTest = withTemporaryTestStorage tempDir ( \_ -> do
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
storeCheckInTest = withTemporaryTestStorage tempDir ( \_ -> do
    -- Store a checkin
    let fpr = Fingerprint "MyFingerprint"
    _ <- storeUserKey fpr
    res <- storeCheckIn fpr (CheckInProof "MyProof")
    case res of
      (Left s) -> assertFailure s
      _ -> return ()
    -- Get a list of checkins
    l <- listCheckIns 10
    -- Verify that only one checkin was returned
    case l of
      (_:[])    -> return ()
      x         -> assertFailure $ "Did not find one checkin: " ++ show x
  )

removeUserKeyTest :: Assertion
removeUserKeyTest = assertFailure "No test here!"
