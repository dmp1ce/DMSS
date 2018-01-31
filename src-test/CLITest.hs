module CLITest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

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
    --   $ dmss-cli id create -n "donald_trump"
    --   $ dmss-cli id list
    --   NAME           EMAIL                         FINGERPRINT
    --   donald_trum...                               D8F162CB21A2A66BC75A4E6DC07272592E46EA59
    --   $ dmss-cli id remove D8F162CB21A2A66BC75A4E6DC07272592E46EA59
    _ <- processIdCreate "donald_trump" "topSecret!"

    -- Get the fingerprint from result of `processIdList`
    l <- processIdList
    length (lines l) @?= 2
    print l

    -- Remove the created user ID
    _ <- processIdRemove "donald_trump"

    -- Simply check that no results are returned
    l' <- processIdList
    print l'
    length (lines l') @?= 1
  )
