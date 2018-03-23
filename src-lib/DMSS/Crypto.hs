-- |
-- Module      : DMSS.Crypto
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System crypto functions
--

--{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}

module DMSS.Crypto where

import DMSS.Storage.Types ( BoxKeypairStore (..)
                          , SignKeypairStore (..)
                          , HashSalt, toHashSalt
                          , CheckInProof, unCheckInProof, mkCheckInProof
                          )


import           Crypto.Lithium.SecretBox ( Key
                                          , secretBox
                                          , getCiphertext
                                          , openSecretBox
                                          )
import qualified Crypto.Lithium.Box as B
import qualified Crypto.Lithium.Sign as S
import qualified Crypto.Lithium.Unsafe.Sign as SU
import qualified Crypto.Lithium.SecretBox as SB
import qualified Crypto.Lithium.Password as P
import           Crypto.Lithium.Unsafe.Types ( Plaintext (..)
                                             , Secret (..)
                                             -- , BytesN
                                             )
import           Data.String (fromString)
import           Data.ByteArray
import           Data.ByteArray.Sized
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Base64 as B64

encryptBoxKeypair :: Key -> B.Keypair -> IO BoxKeypairStore
encryptBoxKeypair symKey (B.Keypair sk pk) = do
  skText <- getCiphertext <$> (secretBox symKey . unSized . reveal . B.unSecretKey) sk
  let pkText = (fromPlaintext . unSized . B.unPublicKey) pk
  return $ BoxKeypairStore (B64.encode skText) (B64.encode pkText)

encryptSignKeypair :: Key -> S.Keypair -> IO SignKeypairStore
encryptSignKeypair symKey (S.Keypair sk pk) = do
  skText <- getCiphertext <$> (secretBox symKey . reveal . S.unSecretKey) sk
  let pkText = (fromPlaintext . S.unPublicKey) pk
  return $ SignKeypairStore (B64.encode skText) (B64.encode pkText)

decryptSignKeypair :: Key -> SignKeypairStore -> Either String S.Keypair
decryptSignKeypair symKey (SignKeypairStore ske pk) =
  let eSK =
        case B64.decode ske of
          Right ske' ->
            case (openSecretBox symKey (SB.SecretBox ske')
                 :: Maybe (Sized SU.SecretKeyBytes ScrubbedBytes)) of
              Just sk -> Right $ S.SecretKey $ Conceal sk
              Nothing -> Left "Failed to decrypt secret key"
          Left e     -> Left e
      ePK = decodeSignPublicKey (SignKeypairStore ske pk)
  in S.Keypair <$> eSK <*> ePK

decodeSignPublicKey :: SignKeypairStore -> Either String S.PublicKey
decodeSignPublicKey (SignKeypairStore _ p) =
  case toPlaintext <$> B64.decode p of
    Right (Just p') -> Right $ S.PublicKey p'
    Right Nothing    -> Left "Failed to decode public key"
    Left e           -> Left e

fromSigned :: S.Signed BS.ByteString -> CheckInProof
fromSigned = mkCheckInProof . S.unSigned

toSigned :: CheckInProof -> S.Signed BS.ByteString
toSigned = S.Signed . unCheckInProof

{- | Create a HashSalt
   This type is our internal representation of a hashed password with a salt
   for deriving a symmetric key.
-}
createHashSalt :: String -> IO HashSalt
createHashSalt p = do
  s <- P.newSalt
  sp <- P.storePassword P.sensitivePolicy (fromString p)
  return $ toHashSalt sp s
