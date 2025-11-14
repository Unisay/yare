{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Serialise.Class.Orphans () where

import Yare.Prelude

import Cardano.Address (Address)
import Cardano.Api qualified as CApi
import Cardano.Chain.Common qualified as Byron
import Cardano.Crypto.Hashing (AbstractHash)
import Cardano.Ledger.Api (Addr, BootstrapAddress (..))
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary qualified as LB
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Ledger
import Codec.CBOR.Decoding qualified as CBOR
import Codec.Serialise.Class (Serialise (..))
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Generics.Orphans ()
import Yare.Chain.Types (ChainTip)

deriving anyclass instance Serialise ChainTip
deriving anyclass instance Serialise Addr
deriving newtype instance Serialise BootstrapAddress
deriving anyclass instance Serialise Address
deriving anyclass instance Serialise (AbstractHash algo a)
deriving anyclass instance Serialise Byron.Address
deriving anyclass instance Serialise Byron.AddrType
deriving anyclass instance Serialise a ⇒ Serialise (Byron.Attributes a)
deriving anyclass instance Serialise Byron.UnparsedFields
deriving anyclass instance Serialise Byron.AddrAttributes
deriving anyclass instance Serialise Byron.HDAddressPayload
deriving anyclass instance Serialise Byron.NetworkMagic
deriving anyclass instance Serialise Ledger.StakeReference
deriving newtype instance Serialise Ledger.SlotNo32
deriving anyclass instance Serialise Ledger.Ptr
deriving anyclass instance Serialise Ledger.Network
deriving anyclass instance Serialise Ledger.TxIx
deriving newtype instance Serialise Ledger.CertIx
deriving anyclass instance Serialise (Ledger.Credential keyRole)
deriving anyclass instance Serialise (Ledger.KeyHash keyRole)
deriving anyclass instance Serialise Ledger.ScriptHash

instance Serialise (HList '[]) where
  encode _ = mempty
  decode = pure HNil

instance
  (Serialise x, Serialise (HList xs))
  ⇒ Serialise (HList (x ': xs))
  where
  encode (HCons x xs) = encode x <> encode xs
  decode = HCons <$> decode <*> decode

deriving newtype instance Serialise a ⇒ Serialise (Tagged tag a)

instance Serialise a ⇒ Serialise (StrictMaybe a)

--------------------------------------------------------------------------------
-- Cardano.Api -----------------------------------------------------------------

deriving anyclass instance Serialise CApi.TxIn

deriving newtype instance Serialise CApi.TxId

deriving newtype instance Serialise CApi.TxIx

instance Serialise CApi.Value where
  encode v = LB.toPlainEncoding maxBound (LB.encCBOR (CApi.toMaryValue v))
  decode = CApi.fromLedgerValue CApi.ShelleyBasedEraConway <$> decodeMaryValue
   where
    decodeMaryValue ∷ CBOR.Decoder s Ledger.MaryValue
    decodeMaryValue = LB.toPlainDecoder Nothing maxBound LB.decCBOR

deriving newtype instance Serialise CApi.ScriptHash
