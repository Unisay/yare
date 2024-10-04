{-# OPTIONS_GHC -Wno-orphans #-}

module NoThunks.Class.Orphans () where

import Yare.Prelude

import Cardano.Address (Address)
import Cardano.Address.Derivation (Depth (PaymentK), XPrv)
import Cardano.Address.Style.Shelley (Shelley)
import Cardano.Api (TxId, TxIn, Value)
import NoThunks.Class (NoThunks (..))

instance NoThunks TxIn where
  wNoThunks _ctx _txIn = pure Nothing
  showTypeOf _proxy = "TxIn"

instance NoThunks TxId where
  noThunks _ctx _txId = pure Nothing
  wNoThunks = noThunks
  showTypeOf _proxy = "TxId"

instance NoThunks Value where
  wNoThunks _ctx _value = pure Nothing
  showTypeOf _proxy = "Value"

instance NoThunks Address where
  wNoThunks _ctx _address = pure Nothing
  showTypeOf _proxy = "Address"

instance NoThunks (Shelley PaymentK XPrv) where
  wNoThunks _ctx _shelleyPaymentKXPrv = pure Nothing
  showTypeOf _proxy = "Shelley PaymentK XPrv"
