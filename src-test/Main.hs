import Test.Tasty
--import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import DaemonTest

main :: IO ()
main = defaultMain $ testGroup "all-tests" Main.tests

tests :: [TestTree]
tests =
  [ testGroup "Daemon tests" DaemonTest.tests
  ]
