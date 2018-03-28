module CLITest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import DMSS.CLI.Internal
import DMSS.CLI ( process
                , Cli (Cli)
                , FlagSilent (SilentOff)
                )
import DMSS.Daemon.Common ( cliPort )
import DMSS.CLI.Command ( Command (Id)
                        , IdCommand (IdCreate, IdList))
import System.Directory (doesPathExist, doesFileExist)

import Common

tests :: [TestTree]
tests =
  [ testCase "create_user_prompt" createUserTest
  , testCase "remove_user_prompt" removeUserTest
  , testCase "data directory created if needed" datadirCreated
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

datadirCreated :: Assertion
datadirCreated = withTemporaryTestDirectory tempDir
  ( \homedir -> do
    let newDatadir = homedir ++ "/test"

    -- Simulate a list user command which will tigger data directory creation.
    process (Cli (Just newDatadir) cliPort SilentOff (Id IdList))
    -- Verify data directory was created
    pathExists <- doesPathExist newDatadir
    assertBool (newDatadir ++ " directory has been created.") pathExists


    -- Simulate a create user command which will tigger data database creation.
    process (Cli (Just newDatadir) cliPort SilentOff (Id (IdCreate (Just "new user") (Just "Password"))))
    -- Verify database was created
    let sqlFile = newDatadir ++ "/.local/share/dmss/dmss.sqlite"
    dataExists <- doesFileExist sqlFile
    assertBool (sqlFile ++ " database has been created.") dataExists
  )
