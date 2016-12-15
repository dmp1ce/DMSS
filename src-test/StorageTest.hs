module StorageTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

--import DMSS.Storage

tests :: [TestTree]
tests =
  [ testCase "store UserKey" storeUserKeyTest
  , testCase "store CheckIn" storeCheckInTest
  , testCase "remove UserKey" removeUserKeyTest
  ]

storeUserKeyTest :: Assertion
storeUserKeyTest = assertFailure "No test here!"

storeCheckInTest :: Assertion
storeCheckInTest = assertFailure "No test here!"

removeUserKeyTest :: Assertion
removeUserKeyTest = assertFailure "No test here!"
