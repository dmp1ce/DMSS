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

import DMSS.Storage ( BoxKeypairStore (..))


import           Crypto.Lithium.SecretBox (Key, secretBox, getCiphertext)
import qualified Crypto.Lithium.Box as B
import           Crypto.Lithium.Unsafe.Types (Plaintext (..), Secret (..))
import           Data.ByteArray.Sized
--import qualified Crypto.Lithium.Sign  as B

encryptBoxKeypair :: Key -> B.Keypair -> IO BoxKeypairStore
encryptBoxKeypair symKey (B.Keypair sk pk) = do
    skText <- getCiphertext <$> ((secretBox symKey) . unSized . reveal . B.unSecretKey) sk
    let pkText = (fromPlaintext . unSized . B.unPublicKey) pk
    return $ BoxKeypairStore skText pkText