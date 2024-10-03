module Yare.Http.Address
  ( Address
  , fromCardanoAddress
  , fromLedgerAddress
  ) where

import Yare.Prelude

import Cardano.Address (bech32)
import Cardano.Address qualified as Cardano
import Data.Aeson (ToJSON)
import Yare.Chain.Types (LedgerAddress, ledgerAddressToText)

type Address ∷ Type
newtype Address = Address Text
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

fromCardanoAddress ∷ Cardano.Address → Address
fromCardanoAddress = Address . bech32

fromLedgerAddress ∷ LedgerAddress → Address
fromLedgerAddress = Address . ledgerAddressToText
