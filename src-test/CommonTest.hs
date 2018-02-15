module CommonTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Common

import DMSS.Common ( toSeconds, toUTCTime )
import DMSS.CLI.Internal ( processIdCreate, processCheckInCreate )
import DMSS.Storage ( listCheckIns, runStorage, verifyPublicCheckIn
                    , Name (Name)
                    )
import Data.String (fromString)

tests :: [TestTree]
tests =
  [ testCase "verify public CheckIn" verifyPublicCheckInTest
  , QC.testProperty "property utcTimeToSeconds and back" prop_utcTimeToSeconds
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
    l <- runStorage $ listCheckIns (Name name) Nothing
    let (checkIn,_) = head l

    -- Verify checkin
    verifyRes <- runStorage $ verifyPublicCheckIn (Name name) checkIn
    assertBool "CheckIn verify failed" verifyRes
  )

prop_utcTimeToSeconds :: Int -> Bool
prop_utcTimeToSeconds s =
  --let utc = UTCTime (ModifiedJulianDay d) (secondsToDiffTime dt)
  s == (toSeconds $ toUTCTime s)
