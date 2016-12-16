import Test.Tasty
--import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import DaemonTest
import StorageTest
import CLITest
import ConfigTest

main :: IO ()
main = defaultMain $ testGroup "all-tests" Main.tests

tests :: [TestTree]
tests =
  [ testGroup "Config tests" ConfigTest.tests
  , testGroup "Storage tests" StorageTest.tests
  , testGroup "Daemon tests" DaemonTest.tests
  , testGroup "CLI tests" CLITest.tests
  ]
