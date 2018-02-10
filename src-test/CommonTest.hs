module CommonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import Common

import DMSS.CLI.Internal ( processIdCreate, processCheckInCreate )
import DMSS.Storage ( listCheckIns, runStorage, verifyPublicCheckIn
                    , Name (Name)
                    )
import Data.String (fromString)

tests :: [TestTree]
tests =
  [ testCase "verify public CheckIn" verifyPublicCheckInTest
  ]

tempDir :: FilePath
tempDir = "commonTest"

verifyPublicCheckInTest :: Assertion
verifyPublicCheckInTest = withTemporaryTestDirectory tempDir ( \_ -> do
    let (name,pass) = ("michael jackson","beat it")

    -- Create Id
    _ <- processIdCreate name pass

    -- Checkin
    _ <- processCheckInCreate (Name name) (fromString pass)

    -- Get last checkin
    l <- runStorage $ listCheckIns (Name name) 1
    let (checkIn,_) = head l

    -- Verify checkin
    verifyRes <- runStorage $ verifyPublicCheckIn (Name name) checkIn
    assertBool "CheckIn verify failed" verifyRes
  )
