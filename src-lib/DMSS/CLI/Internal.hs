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

--import DMSS.Config
import DMSS.Storage ( storeUser
                    , listUsers
                    , removeUser
                    , Name (..)
                    , User ( userHashSalt, userName
                           , userBoxKeypairStore, userSignKeypairStore
                           )
                    , fromHashSalt
                    , mkCheckInProof
                    , BoxKeypairStore (..)
                    , SignKeypairStore (..)
                    , runStorage
                    , storeCheckIn
                    )
--import DMSS.Storage.Types ( toPassHash )
import DMSS.Crypto
import DMSS.Storage.TH ( Unique (..) )
import Database.Esqueleto ( Entity(..)
                          , getBy )

import Data.String.Conv ( toS )
import System.Exit (die)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS8
import qualified Text.PrettyPrint       as PP
import Data.String (fromString)
import qualified Data.Text              as T
import Crypto.Lithium.Password  ( sensitivePolicy
                                , derive
                                , Password (..)
                                , verifyPassword
                                )
import qualified Crypto.Lithium.SecretBox as SB
import qualified Crypto.Lithium.Box       as B
import qualified Crypto.Lithium.Sign      as S

-- | Create a user ID
processIdCreate :: String         -- ^ Name
                -> String         -- ^ Password
                -> IO String
processIdCreate n password = runStorage $ do
  -- Create hash so that I know if the password is correct
  hs <- liftIO $ createHashSalt password

  -- Derive symmetric key with password.
  salt <- liftIO $ either (die . T.unpack) (return . snd) (fromHashSalt hs)

  let symmetricKey = (derive (fromString password) salt sensitivePolicy :: SB.Key)

  -- TODO: Seed could be exported as a mnemonic so identity could be restored on another device
  -- Create keypair seed and encrypt it for later use
  -- seed <- newSeed

  -- Create keypair for encrypting and signing
  kp_box  <- liftIO $ B.newKeypair
  kp_sign <- liftIO $ S.newKeypair
  --let secretBA = B.secretKey kp_box
  --let secretBA' = (UB.fromSecretKey secretBA) :: ByteString
  --kp_sign <- S.newKeypair
  --print secretBA
  --print secretBA'

  --print symmetricKey
  --print passStore

  -- Encrypt private keys for storage and store for later
  kpbStore <- liftIO $ encryptBoxKeypair symmetricKey kp_box
  kpsStore <- liftIO $ encryptSignKeypair symmetricKey kp_sign

  r <- storeUser (Name n) hs kpbStore kpsStore
  return (show r)

-- | List the existing users
processIdList :: IO String
processIdList = do
  users <- listUsers 30
  return $ PP.render (draw users)
  where
    draw :: [User] -> PP.Doc
    draw xs =
      row "NAME" "ENC PUBKEY" "SIGN PUBKEY" PP.$+$ PP.vcat (map dataRow xs)
    row n e f =
      let nameColWidth = 10
          emailColWidth = 48
       in PP.text (ellipsis nameColWidth n)
          PP.$$ PP.nest nameColWidth (PP.text (ellipsis emailColWidth e))
          PP.$$ PP.nest (nameColWidth+emailColWidth) (PP.text f)
    ellipsis n s
      | ((length s) > (n-4)) = (take (n-4) s) ++ "..."
      | otherwise            = s
    dataRow :: User -> PP.Doc
    dataRow u =
      row ((unName . userName) u)
        (BS8.unpack $ (boxPublicKeyStore . userBoxKeypairStore) u)
        (BS8.unpack $ (signPublicKeyStore . userSignKeypairStore) u)

processIdRemove :: String    -- ^ Name
                -> IO (Maybe String)
processIdRemove n = runStorage $ do
  -- Remove from Storage
  removeUser (Name n)
  return Nothing

processCheckInCreate :: Name     -- ^ Name
                     -> Password -- ^ Password
                     -> IO ()
processCheckInCreate n p = runStorage $ do
  -- Verify password is correct for user
  maybeUser <- getBy $ UniqueName n
  user <- case maybeUser of
    Nothing -> liftIO $ die $ "User " ++ (unName n) ++ " not found."
    Just (Entity _ u) -> return u

  (passHash, symmSalt) <- liftIO $ either (die . T.unpack) return $
                                  fromHashSalt $ userHashSalt user
  let symmKey = derive p symmSalt sensitivePolicy
      signKeyStore = (userSignKeypairStore) user
  sMessage <- if verifyPassword passHash p
  -- Decrypt keypair
  then case (decryptSignKeypair symmKey signKeyStore) of
    Right (S.Keypair secr _) -> liftIO $ do
      putStrLn "Unlocked Sign keypair"
      print secr
      putStrLn "Signing and printing test message"
      -- Sign message
      return $ S.sign' secr (toS "Test message here!fefeijfeifjeifejfiejfiefeifjejifeifejifiejeijfj" :: BS8.ByteString)

      --print $ S.unSigned signedMessage
      --putStrLn "Verifying signature"
      --print $ S.openSigned pub signedMessage
    Left e  -> liftIO $ die e
  else liftIO $ die $ "Incorrect password for " ++ (unName n) ++ "."

  _ <- storeCheckIn n (mkCheckInProof $ S.unSigned sMessage)
  return ()

  -- Decrypt Sign keypair with symmetric

  -- Make a signed message proving current time
  -- TODO: Get the latest bitcoin block hash to prove current time

  --eitherCT <- withCtx l "C" OpenPGP $ \ctx -> do
  --  maybeKey <- getKey ctx (C.pack fpr) WithSecret
  --  let key = maybe (error ("Invalid key id " ++ fpr)) id maybeKey
  --  sign ctx [key] Clear (C.pack "Logged in at this date 2016-12-07")
  --let ct = either (\e -> error $ show e) id eitherCT
  --_ <- storeCheckIn (Fingerprint fpr) (CheckInProof $ C.unpack ct)
  --return ()
