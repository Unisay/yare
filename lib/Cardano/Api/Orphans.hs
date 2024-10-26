{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Orphans () where

import Yare.Prelude

import Codec.Serialise.Class.Orphans ()
import Cardano.Api.Shelley
  ( MaryEraOnwards (MaryEraOnwardsConway)
  , ScriptHash
  , ShelleyBasedEra (ShelleyBasedEraConway)
  , TxId (..)
  , TxIn (..)
  , TxIx (..)
  , Value
  , toLedgerValue
  )
import Cardano.Api.Shelley qualified as CApi
import Cardano.Ledger.Binary qualified as LB
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value qualified as Ledger
import Codec.CBOR.Decoding qualified as CBOR
import Codec.Serialise.Class (Serialise (..))

deriving stock instance Generic TxIn
deriving anyclass instance NFData TxIn
deriving anyclass instance Serialise TxIn

deriving newtype instance NFData TxId
deriving newtype instance Serialise TxId

deriving newtype instance NFData TxIx
deriving newtype instance Serialise TxIx

instance Serialise Value where
  encode v = LB.toPlainEncoding maxBound (LB.encCBOR (CApi.toMaryValue v))
  decode = CApi.fromLedgerValue ShelleyBasedEraConway <$> decodeMaryValue
   where
    decodeMaryValue ∷ CBOR.Decoder s (Ledger.MaryValue StandardCrypto)
    decodeMaryValue = LB.toPlainDecoder maxBound LB.decCBOR

instance NFData Value where
  rnf = rnf . toLedgerValue MaryEraOnwardsConway

deriving newtype instance Serialise ScriptHash
