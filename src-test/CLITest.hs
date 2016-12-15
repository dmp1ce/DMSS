module CLITest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

--import DMSS.CLI

tests :: [TestTree]
tests =
  [ testCase "create user" createUserTest
  , testCase "remove user" removeUserTest
  ]

createUserTest :: Assertion
createUserTest = assertFailure "No test here!"

removeUserTest :: Assertion
removeUserTest = assertFailure "No test here!"
