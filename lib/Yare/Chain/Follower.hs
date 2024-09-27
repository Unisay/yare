{-# LANGUAGE TemplateHaskell #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState (..)
  , utxo
  , chainTip
  ) where

import Relude

import Control.Lens.TH (makeLenses)
import NoThunks.Class (NoThunks)
import Ouroboros.Network.Block (Tip (TipGenesis))
import Yare.Addresses (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type ChainFollower ∷ (Type → Type) → Type
data ChainFollower m = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

newChainFollower ∷ Addresses → Storage IO ChainState → ChainFollower IO
newChainFollower addresses storage =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage ((,()) . indexBlock addresses block tip)
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) →
        overStorage storage ((,()) . rollbackTo point tip)
    }

type ChainState ∷ Type
data ChainState = ChainState
  { _utxo ∷ !Utxo
  , _chainTip ∷ !ChainTip
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks)

initialChainState ∷ ChainState
initialChainState =
  ChainState
    { _utxo = Utxo.initial
    , _chainTip = TipGenesis
    }

indexBlock
  ∷ Addresses
  → StdCardanoBlock
  → ChainTip
  → ChainState
  → ChainState
indexBlock addresses block tip cs@ChainState {_utxo} =
  case Utxo.indexBlock addresses block _utxo of
    Nothing → cs {_chainTip = tip}
    Just utxo → trace "UTxO" cs {_utxo = utxo, _chainTip = tip}

rollbackTo ∷ ChainPoint → ChainTip → ChainState → ChainState
rollbackTo point tip cs@ChainState {_utxo} =
  case Utxo.rollbackTo point _utxo of
    Nothing → cs {_chainTip = tip}
    Just utxo → trace "UTxO" cs {_utxo = utxo, _chainTip = tip}

$(makeLenses ''ChainState)
