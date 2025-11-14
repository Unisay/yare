{-# OPTIONS_GHC -Wno-orphans #-}

module Control.DeepSeq.Orphans () where

import Yare.Prelude

import Cardano.Api qualified as CApi
import Cardano.Slotting.Slot ()
import GHC.Generics.Orphans ()
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Network.Block (Point, Tip)
import Ouroboros.Network.Point (Block)
import Yare.Chain.Block (StdCardanoBlock)

--------------------------------------------------------------------------------
-- Ouroboros Network -----------------------------------------------------------

deriving anyclass instance NFData (Point StdCardanoBlock)

deriving anyclass instance (NFData slot, NFData hash) ⇒ NFData (Block slot hash)

deriving newtype instance NFData (OneEraHash hash)

deriving anyclass instance NFData (Tip StdCardanoBlock)

--------------------------------------------------------------------------------
-- HList -----------------------------------------------------------------------

instance NFData (HList '[]) where
  rnf HNil = ()

instance (NFData x, NFData (HList xs)) ⇒ NFData (HList (x ': xs)) where
  rnf (HCons x xs) = rnf x `seq` rnf xs

--------------------------------------------------------------------------------
-- Cardano.Api -----------------------------------------------------------------

deriving anyclass instance NFData CApi.TxIn
deriving newtype instance NFData CApi.TxId
deriving newtype instance NFData CApi.TxIx
deriving newtype instance NFData CApi.ScriptHash
instance NFData CApi.Value where
  rnf = rnf . CApi.toLedgerValue CApi.MaryEraOnwardsConway
