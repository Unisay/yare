-- | Functions to convert to/from Plutus types
module Yare.Compat.Plutus where

import Yare.Prelude

import Cardano.Api.Ledger (Credential (..), KeyHash (..), hashToBytes)
import Cardano.Api.Shelley (TxId (..), TxIn (..), TxIx (..))
import Cardano.Crypto.Hash (Hash)
import Data.IntCast (intCast)
import Plutus.Prelude qualified as Plutus
import Yare.Address.Derivation (toPaymentCredential)
import Yare.Chain.Types (LedgerAddress)

pubKeyHashFromLedgerAddress ∷ LedgerAddress → Maybe Plutus.PubKeyHash
pubKeyHashFromLedgerAddress addr = do
  paymentCredential ← toPaymentCredential addr
  case paymentCredential of
    KeyHashObj (KeyHash keyHash) →
      Just (Plutus.PubKeyHash (hashToBuiltinByteString keyHash))
    ScriptHashObj {} → Nothing

txOutRefFromTxIn ∷ TxIn → Plutus.TxOutRef
txOutRefFromTxIn (TxIn (TxId txId) (TxIx txIx)) =
  Plutus.TxOutRef
    { txOutRefId = Plutus.TxId (hashToBuiltinByteString txId)
    , txOutRefIdx = intCast txIx
    }

hashToBuiltinByteString ∷ Hash crypto w → Plutus.BuiltinByteString
hashToBuiltinByteString = Plutus.toBuiltin . hashToBytes
