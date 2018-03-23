module StorageTest (tests) where

import Data.ByteString.Char8 (pack)
import qualified Database.Persist.Sqlite as P
import DMSS.Storage
  ( storeCheckIn
  , storeUser
  , getUserKey
  , removeUser
  , listCheckIns
  , runStorage
  , Port (Port), Host (Host)
  , storePeer
  , listPeers
  , removePeer
  )
import DMSS.Storage.Types
  ( BoxKeypairStore (..)
  , mkCheckInProof
  , Name (..)
  , HashSalt (HashSalt), fromHashSalt, toHashSalt
  , SignKeypairStore (..)
  , UTCTimeStore
  )
import DMSS.Crypto ( createHashSalt )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( Assertion, assertBool, assertFailure, testCase, (@?=) )
import qualified Test.Tasty.QuickCheck as QC

import Common ( withTemporaryTestDirectory )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

tests :: [TestTree]
tests =
  [ testCase "Store user key test" storeUserTest
  , testCase "Store checkin test" storeCheckInTest
  , testCase "remove_user_key_test" removeUserKeyTest
  , testCase "Store peer test" storePeerTest
  , testCase "Remove peer test" removePeerTest
  , toFromHashSalt
  , QC.testProperty "prop_userStorage_HashSalt"
      prop_userStorage_HashSalt
  , QC.testProperty "prop_userStorage_BoxKeypairStore"
      prop_userStorage_BoxKeypairStore
  , QC.testProperty "prop_userStorage_SignKeypairStore"
      prop_userStorage_SignKeypairStore
  , QC.testProperty "prop_UTCTimeStore" prop_UTCTimeStore
  , QC.testProperty "prop_Port" prop_Port
  ]

tempDir :: FilePath
tempDir = "storageTest"

dummyHashSalt :: HashSalt
dummyHashSalt = HashSalt "password string store" "salt"

dummyBoxKeypairStore :: BoxKeypairStore
dummyBoxKeypairStore = BoxKeypairStore "Box encrypted seckey""Box pubkey"
dummySignKeypairStore :: SignKeypairStore
dummySignKeypairStore = SignKeypairStore "Sign encrypted seckey" "Sign pubkey"

storeUserTest :: Assertion
storeUserTest = withTemporaryTestDirectory tempDir ( \_ -> runStorage $ do
    -- Store fake user key
    let n = Name "joe"
    _ <- storeUser n dummyHashSalt dummyBoxKeypairStore dummySignKeypairStore

    -- Check that the fake user key was stored
    k <- getUserKey n
    case k of
      Nothing -> liftIO $ assertFailure $ "Could not find User based on (" ++ unName n ++ ")"
      _       -> return ()
  )

removeUserKeyTest :: Assertion
removeUserKeyTest = withTemporaryTestDirectory tempDir ( \_ -> runStorage $ do
    -- Store fake user key
    let n = Name "deleteMe1234"
    _ <- storeUser n dummyHashSalt dummyBoxKeypairStore dummySignKeypairStore

    -- Remove key
    removeUser n

    -- Check that the fake user key was stored
    k <- getUserKey n
    case k of
      Nothing -> return ()
      _       -> liftIO $ assertFailure $ "Found UserKey based on (" ++ unName n ++ ") but shouldn't have"
  )

storeCheckInTest :: Assertion
storeCheckInTest = withTemporaryTestDirectory tempDir ( \_ -> runStorage $ do
    -- Store a checkin
    let n = Name "joe"
    _ <- storeUser n dummyHashSalt dummyBoxKeypairStore dummySignKeypairStore
    res <- storeCheckIn n (mkCheckInProof $ pack "MyProof")
    case res of
      (Left s) -> liftIO $ assertFailure s
      _ -> return ()
    -- Get a list of checkins
    l <- listCheckIns n Nothing
    -- Verify that only one checkin was returned
    case l of
      [_] -> return ()
      x   -> liftIO $ assertFailure $ "Did not find one checkin: " ++ show x

    -- Create another checkin and verify order is correct
    _ <- storeCheckIn n (mkCheckInProof $ pack "More proof")
    -- Wait a second to ensure last proof is first on the list
    liftIO $ threadDelay (1000 * 1000)
    let lastProof = mkCheckInProof $ pack "Even more proof"
    _ <- storeCheckIn n lastProof
    l' <- listCheckIns n Nothing
    if fst (head l') == lastProof
      then return ()
      else liftIO $ assertFailure "CheckIns were not in decending order"
  )

storePeerTest :: Assertion
storePeerTest = withTemporaryTestDirectory tempDir
  (\_ -> runStorage $ do
      -- Store fake peer
      _ <- storePeer (Host "locahost") (Port 1000)
  
      -- Check that the fake user key was stored
      ps <- listPeers
      liftIO $ assertBool "No peers stored after adding peer" $ not $ null ps
  )

removePeerTest :: Assertion
removePeerTest = withTemporaryTestDirectory tempDir
  ( \_ -> runStorage $ do
      -- Store fake peer
      _ <- storePeer (Host "locahost") (Port 1000)

      -- Check that the fake user key can be deleted
      removePeer 1
      ps <- listPeers
      liftIO $ assertBool "Peers are still stored after trying to removing last peer" $ null ps
  )

toFromHashSalt :: TestTree
toFromHashSalt = testGroup "Round-trip between PasswordString and HashSalt"
  [ tc ("A short, awful password",  "foobar")
  , tc ("A decent password",        "c%fxBQRe]2L]|#q'")
  , tc ("A passphrase",             "obese page rivet gurgle ring twin usia befit olsen")
  ]
  where
    tc (desc, password) = testCase desc $ do
        hs <- createHashSalt password
        (toHashSalt <$> (fst <$> fromHashSalt hs)
                    <*> (snd <$> fromHashSalt hs))
                @?= Right hs

-- Ensure data that goes into Persistence comes out the same
prop_userStorage_HashSalt :: HashSalt -> Bool
prop_userStorage_HashSalt hs =
  Right hs == (P.fromPersistValue . P.toPersistValue) hs

prop_userStorage_BoxKeypairStore :: BoxKeypairStore -> Bool
prop_userStorage_BoxKeypairStore bkp =
  Right bkp == (P.fromPersistValue . P.toPersistValue) bkp

prop_userStorage_SignKeypairStore :: SignKeypairStore -> Bool
prop_userStorage_SignKeypairStore bkp =
  Right bkp == (P.fromPersistValue . P.toPersistValue) bkp

prop_UTCTimeStore :: UTCTimeStore -> Bool
prop_UTCTimeStore uts = Right uts == (P.fromPersistValue . P.toPersistValue) uts

prop_Port :: Port -> Bool
prop_Port p = Right p == (P.fromPersistValue . P.toPersistValue) p
