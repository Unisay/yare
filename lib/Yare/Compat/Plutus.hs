-- | Functions to convert to/from Plutus types
module Yare.Compat.Plutus where

import Yare.Prelude

import Cardano.Api.Ledger (hashToBytes)
import Cardano.Api
  ( Hash (PaymentKeyHash)
  , PaymentKey
  , TxId (..)
  , TxIn (..)
  , TxIx (..)
  )
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Keys qualified as Ledger
import Data.IntCast (intCast)
import Plutus.Prelude qualified as Plutus

paymentKeyHashToPubKeyHash ∷ Hash PaymentKey → Plutus.PubKeyHash
paymentKeyHashToPubKeyHash (PaymentKeyHash (Ledger.KeyHash keyHash)) =
  Plutus.PubKeyHash (hashToBuiltinByteString keyHash)

txOutRefFromTxIn ∷ TxIn → Plutus.TxOutRef
txOutRefFromTxIn (TxIn (TxId txId) (TxIx txIx)) =
  Plutus.TxOutRef
    { txOutRefId = Plutus.TxId (hashToBuiltinByteString txId)
    , txOutRefIdx = intCast txIx
    }

hashToBuiltinByteString ∷ Crypto.Hash crypto w → Plutus.BuiltinByteString
hashToBuiltinByteString = Plutus.toBuiltin . hashToBytes
