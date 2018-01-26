-- |
-- Module      : DMSS.Storage
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System storage module
--
module DMSS.Storage ( storeCheckIn
                    , listCheckIns
                    , storeUser
                    , getUserKey
                    , removeUser
                    , CheckInId
                    , UserId
                    , CheckInProof (..)
                    , Name (..)
                    , PassHash (..)
                    , dbConnectionString
                    )
  where

import           DMSS.Config ( localDirectory )
import           DMSS.Common ( getCurrentTimeInSeconds )
import           DMSS.Storage.Types
import           DMSS.Storage.TH

import qualified Database.Persist.Sqlite as P
import           Database.Esqueleto
import           Data.Text ( pack
                           , unpack
                           )

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Logger ( NoLoggingT
                                      , runStdoutLoggingT
                                      )
import           Control.Monad.Trans.Resource ( ResourceT )

dbConnectionString :: IO String
dbConnectionString = localDirectory >>= \ld -> pure $ ld ++ "/dmss.sqlite"

-- | Store User information
storeUser :: Name     -- ^ Username
          -> PassHash -- ^ Password Hash
          -> IO (Key User)
storeUser n h = do
  t <- getCurrentTimeInSeconds
  runStorage $ insert $ User n h (KeypairStore "None, yet!") t

removeUser :: Name  -- ^ User to delete by name
           -> IO ()
removeUser _ = undefined --do
  --runStorage $ do
  --  -- Delete all checkins associated with UserKey
  --  mUserKey <- getBy $ UniqueFingerprint fpr
  --  case mUserKey of
  --    Nothing  -> error "Couldn't find UserKey"
  --    (Just uk) -> P.deleteWhere [ UserKeyId P.==. (entityKey uk) ]
  --  -- Then delete UserKey
  --  deleteBy $ UniqueFingerprint fpr

-- | Get UserKey ID
getUserKey :: Silent  -- ^ is silent and no pool?
           -> Name    -- ^ User's name
           -> IO (Maybe (Key User))
getUserKey (Silent True) n = runStorage $ getUserKeyDBActions n
getUserKey (Silent False) n = runStoragePool $ getUserKeyDBActions n

-- | Underlying Database actions for getting UserKey Key
getUserKeyDBActions :: Name -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe (Key User))
getUserKeyDBActions n = do
  maybeUser <- getBy $ UniqueName n
  maybe
    (pure Nothing)
    (\(Entity userId _) -> pure $ Just userId)
    maybeUser

-- | Store a CheckIn
storeCheckIn :: Name          -- ^ Name of users
             -> CheckInProof  -- ^ Raw checkin verification proof
             -> IO (Either String (Key CheckIn))
storeCheckIn n (CheckInProof rawCheckInData) = do
  t <- getCurrentTimeInSeconds
  runStorage $ do
    m <- getUserKeyDBActions n
    maybe (pure $ Left "Could not find users fingerprint in DB")
      (\i -> do
        res <- insert $ CheckIn i rawCheckInData t
        pure $ Right res) m

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Name -> Int -> IO [Entity CheckIn]
listCheckIns n i = runStorage $ do
  s <- select $
         from $ \(u, c) -> do
           where_ ( u ^. UserName ==. val n
                &&. u ^. UserId ==. c ^. CheckInUserId
                  )
           limit (toEnum i)
           orderBy [desc (c ^. CheckInCreated)]
           return c
  return (s :: [Entity CheckIn])

-- | Run storage actions with no logging, no pooling and silent migration
runStorage :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runStorage action = dbConnectionString >>= \c -> P.runSqlite (pack c) $ do
  _ <- runMigrationSilent migrateAll
  action

-- | Run storage actions with stdout logging, pooling and stdout migration
runStoragePool :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runStoragePool action = dbConnectionString >>= \c -> runStdoutLoggingT $ P.withSqlitePool (pack c) 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    stuff <- runMigrationSilent migrateAll
    liftIO $ mapM_ (putStrLn . unpack) stuff
    action
