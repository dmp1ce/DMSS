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

import DMSS.Storage ( BoxKeypairStore (..)
                    , SignKeypairStore (..)
                    , HashSalt, toHashSalt
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
import qualified Data.ByteString.Base64 as B64

encryptBoxKeypair :: Key -> B.Keypair -> IO BoxKeypairStore
encryptBoxKeypair symKey (B.Keypair sk pk) = do
  skText <- getCiphertext <$> ((secretBox symKey) . unSized . reveal . B.unSecretKey) sk
  let pkText = (fromPlaintext . unSized . B.unPublicKey) pk
  return $ BoxKeypairStore (B64.encode skText) (B64.encode pkText)

encryptSignKeypair :: Key -> S.Keypair -> IO SignKeypairStore
encryptSignKeypair symKey (S.Keypair sk pk) = do
  skText <- getCiphertext <$> ((secretBox symKey) . reveal . S.unSecretKey) sk
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
      ePK = case toPlaintext <$> B64.decode pk of
              Right (Just pk') -> Right $ S.PublicKey pk'
              Right Nothing    -> Left "Failed to decode public key"
              Left e           -> Left e
   in S.Keypair <$> eSK <*> ePK

{- | Create a HashSalt
   This type is our internal representation of a hashed password with a salt
   for deriving a symmetric key.
-}
createHashSalt :: String -> IO HashSalt
createHashSalt p = do
  s <- P.newSalt
  sp <- P.storePassword P.sensitivePolicy (fromString p)
  return $ toHashSalt sp s
