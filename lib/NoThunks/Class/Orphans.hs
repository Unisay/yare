{-# OPTIONS_GHC -Wno-orphans #-}

module NoThunks.Class.Orphans () where

import Relude

import Cardano.Api (TxIn, Value)
import NoThunks.Class (NoThunks (..))

instance NoThunks TxIn where
  wNoThunks _ctx _txIn = pure Nothing
  showTypeOf _proxy = "TxIn"

instance NoThunks Value where
  wNoThunks _ctx _value = pure Nothing
  showTypeOf _proxy = "Value"
