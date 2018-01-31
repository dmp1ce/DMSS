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


import           Crypto.Lithium.SecretBox (Key, secretBox, getCiphertext)
import qualified Crypto.Lithium.Box as B
import qualified Crypto.Lithium.Sign as S
import           Crypto.Lithium.Unsafe.Types (Plaintext (..), Secret (..))
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
    skText <- getCiphertext <$> ((secretBox symKey) . unSized . reveal . S.unSecretKey) sk
    let pkText = (fromPlaintext . unSized . S.unPublicKey) pk
    return $ SignKeypairStore (B64.encode skText) (B64.encode pkText)
