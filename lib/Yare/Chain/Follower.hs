{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState
  , HasChainState
  ) where

import Yare.Prelude

import Data.Row.Records qualified as Rec
import Fmt (pretty)
import Ouroboros.Network.Block (Tip (TipGenesis))
import Relude.Extra (dup)
import Relude.Extra.Tuple (toFst)
import Yare.Address (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

data ChainFollower (m ∷ Type → Type) = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

type ChainStateRow = "utxo" .== Utxo .+ "chainTip" .== ChainTip
type ChainState = Rec ChainStateRow
type HasChainState r = Open ChainStateRow r

newChainFollower
  ∷ ( HasChainState r
    , HasType "addresses" Addresses r
    , state ≈ Rec r
    )
  ⇒ Storage IO state
  → ChainFollower IO
newChainFollower storage =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage (indexBlock block tip) \case
          Nothing → pass -- UTxO was not updated
          Just updatedUtxo → putStrLn $ pretty updatedUtxo
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) →
        overStorage storage (dup . rollbackTo point tip) do
          putStrLn . pretty . (.! #utxo)
    }

initialChainState ∷ ChainState
initialChainState = #utxo .== Utxo.initial .+ #chainTip .== TipGenesis

indexBlock
  ∷ ( HasChainState r
    , HasType "addresses" Addresses r
    , state ≈ Rec r
    )
  ⇒ StdCardanoBlock
  → ChainTip
  → state
  → (state, Maybe Utxo)
indexBlock block tip state =
  Utxo.indexBlock (state .! #addresses) block (state .! #utxo)
    & toFst \case
      Nothing → Rec.update #chainTip tip state
      Just utxo' →
        state
          & Rec.update #utxo utxo'
          & Rec.update #chainTip tip

rollbackTo
  ∷ (HasChainState r, state ≈ Rec r)
  ⇒ ChainPoint
  → ChainTip
  → state
  → state
rollbackTo point tip chainState =
  case Utxo.rollbackTo point (chainState .! #utxo) of
    Nothing → Rec.update #chainTip tip chainState
    Just utxo →
      chainState
        & Rec.update #utxo utxo
        & Rec.update #chainTip tip
