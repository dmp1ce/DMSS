-- |
-- Module      : DMSS.Storage.Types
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System storage types
--
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DMSS.Storage.Types where

import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserKey
  fingerprint String -- ^ Fingerprint of GPG key
  UniqueFingerprint fingerprint
  created Int -- ^ POSIX time
CheckIn
  userId UserKeyId
  raw_data String
  created Int -- ^ POSIX time
  deriving Show
|]

newtype Fingerprint  = Fingerprint  { unFingerprint :: String }
newtype CheckInProof = CheckInProof { unCheckInProof :: String }
