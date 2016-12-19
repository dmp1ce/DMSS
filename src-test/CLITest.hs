module CLITest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import DMSS.CLI
import DMSS.Command

import Common

tests :: [TestTree]
tests =
  [ testCase "create_user_prompt" createUserTest
  , testCase "remove_user" removeUserTest
  ]

tempDir :: FilePath
tempDir = "cliTest"

createUserTest :: Assertion
createUserTest = withTemporaryTestStorage tempDir ( \_ -> do
    -- Simulate a create user command.
    -- For example: dmss-cli id create -n "joe blow"
    let c = Cli (Id (IdCreate (Just "joe blow") (Just "joeblow@example.com")))
    process c

    assertFailure "Need to check for existance of user"
  )

removeUserTest :: Assertion
removeUserTest = assertFailure "No test here!"
