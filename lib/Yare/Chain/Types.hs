module Yare.Chain.Types
  ( ChainTip
  , ChainPoint
  , parseChainPoint
  , LedgerAddress
  ) where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Crypto (StandardCrypto)
import Data.Kind (Type)
import Ouroboros.Network.Block (Tip)
import Yare.Chain.Block (HFBlock)
import Yare.Chain.Point (ChainPoint, parseChainPoint)

type ChainTip ∷ Type
type ChainTip = Tip HFBlock

type LedgerAddress ∷ Type
type LedgerAddress = Addr StandardCrypto
