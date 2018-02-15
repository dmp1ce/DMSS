module CLITest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import DMSS.CLI.Internal

import Common

tests :: [TestTree]
tests =
  [ testCase "create_user_prompt" createUserTest
  , testCase "remove_user_prompt" removeUserTest
  ]

tempDir :: FilePath
tempDir = "cliTest"

createUserTest :: Assertion
createUserTest = withTemporaryTestDirectory tempDir ( \_ -> do
    -- Simulate a create user command.
    -- For example: dmss-cli id create -n "joe blow"
    _ <- processIdCreate "joe blow" "password"

    -- Simply check that the header and one entry is returned
    l <- processIdList
    length (lines l) @?= 2
  )

removeUserTest :: Assertion
removeUserTest = withTemporaryTestDirectory tempDir ( \_ -> do
    -- Simulate a create user and then removing a user.
    -- For example:
    --   $ dmss-cli id create -n "donald_trump" -p "password"
    --   $ dmss-cli id list
    --   NAME
    --   donald_trum...
    --   $ dmss-cli id remove donald_trump
    _ <- processIdCreate "donald_trump" "topSecret!"

    -- Get the fingerprint from result of `processIdList`
    l <- processIdList
    length (lines l) @?= 2

    -- Remove the created user ID
    _ <- processIdRemove "donald_trump"

    -- Simply check that no results are returned
    l' <- processIdList
    length (lines l') @?= 1
  )
