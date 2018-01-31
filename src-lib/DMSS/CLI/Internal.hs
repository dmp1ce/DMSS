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
                    , PassHash (..)
                    , User (..)
                    , BoxKeypairStore (..)
                    , SignKeypairStore (..)
                    )
--                    , storeCheckIn
--                    , Fingerprint (..)
--                    , CheckInProof (..)
--                   )
import DMSS.Crypto

--import Data.Maybe (fromJust)
--import Text.Email.Validate
--import Data.Default (def)
import qualified Data.ByteString.Char8  as BS8
import qualified Text.PrettyPrint       as PP
import Data.String (fromString)
import Crypto.Lithium.Password  ( storePassword
                                , sensitivePolicy
                                , newSalt
                                , derive
                                )
import qualified Crypto.Lithium.SecretBox as SB
import qualified Crypto.Lithium.Box       as B
import qualified Crypto.Lithium.Sign      as S
--import qualified Crypto.Lithium.Unsafe.Box  as UB
--import qualified Crypto.Lithium.Sign as S
--import Data.ByteString

--import Crypto.Lithium.Password
--import Crypto.Lithium.Box

-- | Create a user ID
processIdCreate :: String         -- ^ Name
                -> String         -- ^ Password
                -> IO String
processIdCreate n password = do
  -- Create hash so that I know if the password is correct
  passStore <- storePassword sensitivePolicy (fromString password)

  -- Derive symmetric key with password.
  salt <- newSalt
  let symmetricKey = (derive (fromString password) salt sensitivePolicy :: SB.Key)

  -- TODO: Seed could be exported as a mnemonic so identity could be restored on another device
  -- Create keypair seed and encrypt it for later use
  -- seed <- newSeed

  -- Create keypair for encrypting and signing
  kp_box  <- B.newKeypair
  kp_sign <- S.newKeypair
  --let secretBA = B.secretKey kp_box
  --let secretBA' = (UB.fromSecretKey secretBA) :: ByteString
  --kp_sign <- S.newKeypair
  --print secretBA
  --print secretBA'

  --print symmetricKey
  --print passStore

  -- Encrypt private keys for storage and store for later
  kpbStore <- encryptBoxKeypair symmetricKey kp_box
  kpsStore <- encryptSignKeypair symmetricKey kp_sign
  r <- storeUser (Name n) (PassHash (show passStore)) kpbStore kpsStore
  return (show r)

--processIdCreate n e = do
--  -- Create GPG id
--  l <- gpgContext
--  let params = (def :: G.GenKeyParams)
--        { G.keyType = Just Dsa
--        , G.nameReal = C.pack n
--        , G.nameEmail = (emailAddress . C.pack) $ maybe "" id e
--        }
--  createLocalDirectory
--  ret <- withCtx l "C" OpenPGP $ \ctx -> do
--    eitherFpr <- G.genKey ctx params
--    either (\_ -> return ("genKey failed with return: " ++ show eitherFpr))
--      (\fpr -> do
--          s <- storeUserKey $ Fingerprint $ C.unpack fpr
--          return $ show s)
--      eitherFpr
--  return $ show ret

-- | List the existing users
processIdList :: IO String
processIdList = do
  -- TODO: Remove hardcoded max size
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
processIdRemove n = do
  -- Remove from Storage
  removeUser (Name n)
  return Nothing

  --l <- gpgContext
  --ret <- withCtx l "C" OpenPGP $ \ctx -> do
  --  key <- getKey ctx (C.pack fpr) WithSecret

  --  -- Remove from GPG context
  --  removeKey ctx (fromJust key) WithSecret
  --case ret of
  --  Nothing  -> return $ Nothing
  --  (Just e) -> return $ Just $ show e

processCheckInCreate :: String -- ^ Name
                     -> String -- ^ Password
                     -> IO ()
processCheckInCreate _ = undefined --do
  --l <- gpgContext
  --eitherCT <- withCtx l "C" OpenPGP $ \ctx -> do
  --  maybeKey <- getKey ctx (C.pack fpr) WithSecret
  --  let key = maybe (error ("Invalid key id " ++ fpr)) id maybeKey
  --  sign ctx [key] Clear (C.pack "Logged in at this date 2016-12-07")
  --let ct = either (\e -> error $ show e) id eitherCT
  --_ <- storeCheckIn (Fingerprint fpr) (CheckInProof $ C.unpack ct)
  --return ()
