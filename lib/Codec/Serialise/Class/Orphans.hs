{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Serialise.Class.Orphans () where

import Yare.Prelude

import Cardano.Address (Address)
import Cardano.Chain.Common qualified as Byron
import Cardano.Crypto.Hashing (AbstractHash)
import Cardano.Ledger.Api (Addr, BootstrapAddress (..))
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Codec.Serialise.Class (Serialise (..))
import Yare.Chain.Types (ChainTip)

deriving anyclass instance Serialise ChainTip
deriving anyclass instance Serialise (Addr crypto)
deriving newtype instance Serialise (BootstrapAddress crypto)
deriving anyclass instance Serialise Address
deriving anyclass instance Serialise (AbstractHash algo a)
deriving anyclass instance Serialise Byron.Address
deriving anyclass instance Serialise Byron.AddrType
deriving anyclass instance Serialise a ⇒ Serialise (Byron.Attributes a)
deriving anyclass instance Serialise Byron.UnparsedFields
deriving anyclass instance Serialise Byron.AddrAttributes
deriving anyclass instance Serialise Byron.HDAddressPayload
deriving anyclass instance Serialise Byron.NetworkMagic
deriving anyclass instance Serialise (Ledger.StakeReference crypto)
deriving anyclass instance Serialise Ledger.Ptr
deriving anyclass instance Serialise Ledger.Network
deriving anyclass instance Serialise Ledger.TxIx
deriving newtype instance Serialise Ledger.CertIx
deriving anyclass instance Serialise (Ledger.Credential keyRole crypto)
deriving anyclass instance Serialise (Ledger.KeyHash keyRole crypto)
deriving anyclass instance Serialise (Ledger.ScriptHash crypto)

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

