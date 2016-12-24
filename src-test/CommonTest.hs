module CommonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import Common

tests :: [TestTree]
tests =
  [ testCase "verify_checkin" verifyCheckIn
  ]

tempDir :: FilePath
tempDir = "commonTest"

verifyCheckIn :: Assertion
verifyCheckIn = withTemporaryAliceHome tempDir ( \_ -> do
    -- Create user
    -- Create checkin
    -- Get last checkin
    -- Verify checkin
    assertFailure "Not implemented"
  )

