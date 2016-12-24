-- |
-- Module      : DMSS.CLI.Internal
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System CLI module for internal only functions.
-- This module is used to seperate stdout from String output for testing purposes.
--

module DMSS.CLI.Internal where

import DMSS.Config
import DMSS.Storage ( storeUserKey
                    , removeUserKey
                    , Fingerprint (..)
                    )

import Crypto.Gpgme
import qualified  Crypto.Gpgme.Key.Gen   as G

import Data.Maybe (fromJust)
import Text.Email.Validate
import Data.Default (def)
import qualified Data.ByteString.Char8   as C
import qualified  Text.PrettyPrint       as PP

-- | Create a user ID
processIdCreate :: String         -- ^ Name
                -> Maybe String   -- ^ Email
                -> IO String      -- ^ CLI output
processIdCreate n e = do
    -- Create GPG id
    l <- gpgContext
    let params = (def :: G.GenKeyParams)
          { G.keyType = Just Dsa
          , G.nameReal = C.pack n
          , G.nameEmail = (emailAddress . C.pack) $ maybe "" id e
          }
    createLocalDirectory
    ret <- withCtx l "C" OpenPGP $ \ctx -> do
      eitherFpr <- G.genKey ctx params
      either (\_ -> return ("genKey failed with return: " ++ show eitherFpr))
        (\fpr -> do
            s <- storeUserKey $ Fingerprint $ C.unpack fpr
            return $ show s)
        eitherFpr
    return $ show ret

-- | List the existing use IDs
processIdList :: IO String
processIdList = do
  l <- gpgContext
  res <- withCtx l "C" OpenPGP $ \ctx ->
    listKeys ctx WithSecret
  ids <- mapM (\k -> do
              i <- keyUserIds' k
              s <- keySubKeys' k
              return (i,s)
            ) res
  return $ PP.render (draw $ map (\i -> (fst i, snd i)) ids)
  where
    draw :: [([KeyUserId], [SubKey])] -> PP.Doc
    draw xs =
      row "NAME" "EMAIL" "FINGERPRINT" PP.$+$ PP.vcat (map dataRow xs)
    row n e f =
      let nameColWidth = 15
          emailColWidth = 30
       in PP.text (ellipsis nameColWidth n)
          PP.$$ PP.nest nameColWidth (PP.text (ellipsis emailColWidth e))
          PP.$$ PP.nest (nameColWidth+emailColWidth) (PP.text f)
    ellipsis n s
      | ((length s) > (n-4)) = (take (n-4) s) ++ "..."
      | otherwise            = s
    dataRow :: ([KeyUserId], [SubKey]) -> PP.Doc
    dataRow (names, subkeys) =
      row (cc (userName . keyuserId) names)
        (cc (userEmail . keyuserId) names)
        (cc (C.unpack . subkeyFpr) subkeys)
      where
        cc f l = unwords (foldr (\n a -> (f n):a) [] l)

processIdRemove :: String    -- ^ Fingerprint
                -> IO (Maybe String)
processIdRemove fpr = do
  -- Remove from Storage
  removeUserKey (Fingerprint fpr)

  l <- gpgContext
  ret <- withCtx l "C" OpenPGP $ \ctx -> do
    key <- getKey ctx (C.pack fpr) WithSecret

    -- Remove from GPG context
    removeKey ctx (fromJust key) WithSecret
  case ret of
    Nothing  -> return $ Nothing
    (Just e) -> return $ Just $ show e
