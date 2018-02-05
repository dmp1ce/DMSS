{-# LANGUAGE OverloadedStrings #-}

module BlockTimeTest (tests) where

import DMSS.BlockTime.BlockTime ( getBlockTime, getLatestBlock )
import DMSS.BlockTime.Types ( BTCBlock (blHash) )

import Test.Tasty ( TestTree )
import Test.Tasty.HUnit ( Assertion, assertFailure, testCase )
import Text.Printf ( printf )



tests :: [TestTree]
tests =
  [ testCase "get info about latest Bitcoin block" blockTimeTest
  ]


blockTimeTest :: Assertion
blockTimeTest = do
  eLatestBlock <- getLatestBlock
  putStrLn $ "\n      Latest BTC block: " ++ (show eLatestBlock)

  case eLatestBlock of
    Right lb -> do
      let hash = blHash lb
      eTimeOfBlock <- getBlockTime hash
      printf "      Timestamp of block %s: %s\n" hash (show eTimeOfBlock)
    Left e -> assertFailure . show $ e
