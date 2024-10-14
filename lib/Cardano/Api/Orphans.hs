{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Orphans () where

import Cardano.Api.Shelley
import Codec.Serialise.Class (Serialise (..))
import Yare.Prelude

deriving stock instance Generic TxIn
deriving anyclass instance NFData TxIn
deriving anyclass instance Serialise TxIn

deriving newtype instance NFData TxId
deriving newtype instance Serialise TxId

deriving newtype instance NFData TxIx
deriving newtype instance Serialise TxIx

instance Serialise Value where
  encode v = $notImplemented
  decode = $notImplemented

instance NFData Value where
  rnf = rnf . toLedgerValue MaryEraOnwardsConway
