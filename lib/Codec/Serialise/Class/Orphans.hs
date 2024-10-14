{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.Serialise.Class.Orphans () where

import Yare.Prelude

import Cardano.Address (Address)
import Cardano.Address.Derivation (Depth (PaymentK), XPrv)
import Cardano.Address.Style.Shelley qualified as CAddr
import Cardano.Chain.Common qualified as Byron
import Cardano.Crypto.Hashing (AbstractHash)
import Cardano.Crypto.Wallet qualified as CC
import Cardano.Ledger.Api (Addr, BootstrapAddress (..))
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Codec.Serialise.Class (Serialise (..))
import Data.Strict.List qualified as Strict
import Yare.Chain.Types (ChainTip)

instance Forall r Serialise ⇒ Serialise (Rec r) where
  encode = $notImplemented
  decode = $notImplemented

instance Serialise a ⇒ Serialise (Strict.List a) where
  encode = encodeList . toList
  decode = fromList <$> decodeList

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
deriving anyclass instance Serialise (CAddr.Shelley 'PaymentK XPrv)

instance Serialise XPrv where
  encode = encode . CC.unXPrv
  decode = $notImplemented
