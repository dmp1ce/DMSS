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
module DMSS.Crypto where

import DMSS.Storage ( BoxKeypairStore (..)
                    , SignKeypairStore (..)
                    )


import           Crypto.Lithium.SecretBox ( Key
                                          , secretBox
                                          , getCiphertext
                                          , openSecretBox
                                          )
import qualified Crypto.Lithium.Box as B
import qualified Crypto.Lithium.Sign as S
import qualified Crypto.Lithium.SecretBox as SB
import           Crypto.Lithium.Unsafe.Types ( Plaintext (..)
                                             , Secret (..)
                                             -- , BytesN
                                             )
import           Data.ByteArray
import           Data.ByteArray.Sized
import qualified Data.ByteString.Base64 as B64
--import qualified Crypto.Lithium.Sign  as B

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
  case B64.decode ske of
    Right ske' ->
      -- :: Maybe (Sized n ScrubbedBytes) of
      case openSecretBox symKey (SB.SecretBox ske') of
        Just k -> undefined
        Nothing -> Left undefined
    Left e     -> Left e
