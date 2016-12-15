import Test.Tasty
--import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import DaemonTest
import StorageTest
import CLITest

main :: IO ()
main = defaultMain $ testGroup "all-tests" Main.tests

tests :: [TestTree]
tests =
  [ testGroup "Daemon tests" DaemonTest.tests
  , testGroup "Storage tests" StorageTest.tests
  , testGroup "CLI tests" CLITest.tests
  ]
