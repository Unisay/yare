{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState
  ) where

import Yare.Prelude

import Cardano.Slotting.Slot (fromWithOrigin)
import Data.Row.Records qualified as Rec
import Ouroboros.Network.Block
  ( BlockNo (..)
  , SlotNo (..)
  , Tip (..)
  , blockNo
  , blockSlot
  , getTipBlockNo
  )
import Relude.Extra (dup)
import Relude.Extra.Tuple (toFst)
import Yare.Address (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Tracer (Tracer, traceWith)
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

data ChainFollower (m ∷ Type → Type) = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

type ChainStateRow = "utxo" .== Utxo .+ "chainTip" .== ChainTip
type ChainState = Rec ChainStateRow

newChainFollower
  ∷ ( HasType "utxo" Utxo r
    , HasType "chainTip" ChainTip r
    , HasType "addresses" Addresses r
    , state ≈ Rec r
    )
  ⇒ Tracer IO Utxo
  → Tracer IO SlotNo
  → Storage IO state
  → ChainFollower IO
newChainFollower utxoTracer progressTracer storage =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage (indexBlock block tip) \case
          Nothing → do
            -- UTxO was not updated
            let slotNo@(SlotNo slot) = blockSlot block
            -- Trace progress every 100th slot
            when (slot `mod` 100 == 0) do
              traceWith progressTracer slotNo
          Just updatedUtxo →
            traceWith utxoTracer updatedUtxo
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) →
        overStorage storage (dup . rollbackTo point tip) do
          traceWith utxoTracer . (.! #utxo)
    }

initialChainState ∷ ChainState
initialChainState = #utxo .== Utxo.initial .+ #chainTip .== TipGenesis

indexBlock
  ∷ ( HasType "utxo" Utxo r
    , HasType "chainTip" ChainTip r
    , HasType "addresses" Addresses r
    , state ≈ Rec r
    )
  ⇒ StdCardanoBlock
  → ChainTip
  → state
  → (state, Maybe Utxo)
indexBlock block tip state =
  Utxo.indexBlock (state .! #addresses) block blockIsFinal utxo
    & toFst \case
      Nothing →
        state
          & Rec.update #chainTip tip
      Just utxo' →
        state
          & Rec.update #utxo utxo'
          & Rec.update #chainTip tip
 where
  utxo = state .! #utxo
  BlockNo tipBlockNo = fromWithOrigin 0 (getTipBlockNo tip)
  BlockNo blockHeight = blockNo block
  blockIsFinal = blockHeight < tipBlockNo - securityParam

rollbackTo
  ∷ ( HasType "utxo" Utxo r
    , HasType "chainTip" ChainTip r
    , state ≈ Rec r
    )
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

{- | The security parameter is a non-updatable one:
After how many blocks is the blockchain considered to be final,
and thus can no longer be rolled back
(i.e. what is the maximum allowable length of any chain fork).
-}
securityParam ∷ Word64
securityParam = 2160
