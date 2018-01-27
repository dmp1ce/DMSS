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


import Crypto.Lithium.SecretBox (Key)
import qualified Crypto.Lithium.Box  as B
--import qualified Crypto.Lithium.Sign  as B

encryptBoxKeypair :: Key -> B.Keypair -> BoxKeypairStore
encryptBoxKeypair = undefined
