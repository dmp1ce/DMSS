module CommonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import Common

import DMSS.CLI.Internal ( processIdCreate, processCheckInCreate )
import DMSS.Storage ( listCheckIns, runStorage )
import DMSS.Common
import DMSS.Storage.Types
import Data.String (fromString)

tests :: [TestTree]
tests =
  [ testCase "verify_checkin_prompt" verifyCheckInTest
  ]

tempDir :: FilePath
tempDir = "commonTest"

verifyCheckInTest :: Assertion
verifyCheckInTest = withTemporaryTestDirectory tempDir ( \_ -> do
    let (name,pass) = ("michael jackson","beat it")

    -- Create Id
    _ <- processIdCreate name pass

    -- Checkin
    _ <- processCheckInCreate (Name name) (fromString pass)

    -- Get last checkin
    l <- runStorage $ listCheckIns (Name name) 1
    --print l
    let checkIn = head l

    -- Verify checkin
    verifyRes <- verifyCheckIn (Name name) (DMSS.Storage.Types.Password pass) checkIn
    assertBool "CheckIn verify failed" verifyRes

  --  let fpr = "EF86E97B41918B7E7E939FA7DAD31A050AC8E53E"
  --  -- Create checkin
  --  processCheckInCreate fpr
  --  -- Get last checkin
  --  l <- listCheckIns (Fingerprint fpr) 1
  --  --print l
  --  let checkIn = head l

  --  -- Verify checkin
  --  verifyRes <- verifyCheckIn (Fingerprint fpr) checkIn
  --  assertBool "CheckIn was not verified" verifyRes
  )
