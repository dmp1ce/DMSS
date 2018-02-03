{-# LANGUAGE OverloadedStrings #-}

module BlockTimeTest (tests) where

import DMSS.BlockTime ( getBlockTime, getLatestBlock )

import Test.Tasty ( TestTree )
import Test.Tasty.HUnit ( Assertion, assertFailure, testCase )



tests :: [TestTree]
tests =
  [ testCase "get latest Bitcoin block" getLatestBlockTest
  , testCase "get timestamp for a block" getBlockTimeTest
  ]


getLatestBlockTest :: Assertion
getLatestBlockTest = do
  actual <- getLatestBlock

  case actual of
    Right block -> print block
    Left e -> assertFailure . show $ e


getBlockTimeTest :: Assertion
getBlockTimeTest = do
  actual <- getBlockTime
    "00000000000000000062d9da0f214fc674fdd60cfdfd745c2b5a4baff91987ed"

  case actual of
    Right time -> print time
    Left e -> assertFailure . show $ e
