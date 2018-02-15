import Test.Tasty

import DaemonTest
import StorageTest
import CLITest
import ConfigTest
import CommonTest

main :: IO ()
main = defaultMain $ testGroup "all-tests" Main.tests

tests :: [TestTree]
tests =
  [ testGroup "config"  ConfigTest.tests
  , testGroup "storage" StorageTest.tests
  , testGroup "daemon"  DaemonTest.tests
  , testGroup "cli"     CLITest.tests
  , testGroup "common"  CommonTest.tests
  ]
