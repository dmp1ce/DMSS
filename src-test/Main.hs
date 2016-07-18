import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Lib (inc)

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [ testProperty "inc == succ" prop_succ
  , testProperty "inc . negate == negate . pred" prop_pred
  ]

huTests :: [TestTree]
huTests =
  [ testCase "Increment below TheAnswer" case_inc_below
  , testCase "Decrement above TheAnswer" case_dec_above
  ]

prop_succ :: Int -> Bool
prop_succ n = inc n == succ n

prop_pred :: Int -> Bool
prop_pred n = inc (negate n) == negate (pred n)

case_inc_below :: Assertion
case_inc_below = inc 41 @?= (42 :: Int)

case_dec_above :: Assertion
case_dec_above = negate (inc (negate 43)) @?= (42 :: Int)
