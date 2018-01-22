module CommonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

--import Common

--import DMSS.CLI.Internal
--import DMSS.Storage
--import DMSS.Common

tests :: [TestTree]
tests =
  [ testCase "verify_checkin_prompt" verifyCheckInTest
  ]

--tempDir :: FilePath
--tempDir = "commonTest"

verifyCheckInTest :: Assertion
verifyCheckInTest = undefined
  -- Need a new fixture, or a way to generate environment for testing checkins
  --withTemporaryAliceHome tempDir ( \_ -> do
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
  --)
