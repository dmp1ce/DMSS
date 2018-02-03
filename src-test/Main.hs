import Test.Tasty
--import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import BlockTimeTest
{-
import DaemonTest
import CLITest
import CommonTest
import ConfigTest
import StorageTest
-}


main :: IO ()
main = defaultMain $ testGroup "all-tests" Main.tests


tests :: [TestTree]
tests =
  {-
  [ testGroup "config"      ConfigTest.tests
  , testGroup "storage"     StorageTest.tests
  , testGroup "daemon"      DaemonTest.tests
  , testGroup "cli"         CLITest.tests
  , testGroup "common"      CommonTest.tests
  -}
  [ testGroup "block time"  BlockTimeTest.tests
  ]
