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

import Cardano.Api.Shelley (TxId)
import Cardano.Slotting.Slot (fromWithOrigin)
import Control.Monad.Class.MonadThrow (throwIO)
import Data.Row.Records qualified as Rec
import Data.Strict (List)
import Ouroboros.Network.Block
  ( BlockNo (..)
  , SlotNo (..)
  , Tip (..)
  , blockNo
  , blockSlot
  , getTipBlockNo
  )
import Relude.Extra (dup)
import Yare.Address (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Tracer (Tracer, traceWith)
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo
import Yare.Utxo.Indexer (UtxoUpdate (..))
import Yare.Utxo.Indexer qualified as Utxo

data ChainFollower (m ∷ Type → Type) = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

type ChainStateRow = "utxo" .== Utxo .+ "chainTip" .== ChainTip
type ChainState = Rec ChainStateRow

newChainFollower
  ∷ ∀ r env state
   . ( HasType "utxo" Utxo r
     , HasType "chainTip" ChainTip r
     , HasType "addresses" Addresses r
     , HasType "submitted" (List TxId) r
     , state ≈ Rec r
     , HasType "tracerUtxo" (Tracer IO Utxo) env
     , HasType "tracerTxId" (Tracer IO TxId) env
     , HasType "tracerSync" (Tracer IO SlotNo) env
     , HasType "tracerRollback" (Tracer IO ChainPoint) env
     )
  ⇒ Rec env
  → Storage IO state
  → ChainFollower IO
newChainFollower env storage =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage (indexBlock block tip) \case
          UtxoNotUpdated →
            traceWith (env .! #tracerSync) (blockSlot block)
          UtxoUpdated updatedUtxo txIds → do
            for_ txIds $ traceWith (env .! #tracerTxId)
            traceWith (env .! #tracerUtxo) updatedUtxo
          UtxoUpdateError err →
            throwIO err
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) → do
        traceWith (env .! #tracerRollback) point
        overStorage storage (dup . rollbackTo point tip) do
          traceWith (env .! #tracerUtxo) . (.! #utxo)
    }

initialChainState ∷ ChainState
initialChainState = #utxo .== Utxo.initial .+ #chainTip .== TipGenesis

indexBlock
  ∷ ∀ r state
   . ( HasType "utxo" Utxo r
     , HasType "chainTip" ChainTip r
     , HasType "addresses" Addresses r
     , HasType "submitted" (List TxId) r
     , state ≈ Rec r
     )
  ⇒ StdCardanoBlock
  → ChainTip
  → state
  → (state, UtxoUpdate)
indexBlock block tip state = (state', utxoUpdate)
 where
  state' ∷ state =
    case utxoUpdate of
      UtxoUpdateError {} →
        state
          & Rec.update #chainTip tip
      UtxoNotUpdated →
        state
          & Rec.update #chainTip tip
      UtxoUpdated utxo' _txIds →
        state
          & Rec.update #utxo utxo'
          & Rec.update #chainTip tip

  utxoUpdate ∷ UtxoUpdate =
    Utxo.indexBlock
      (state .! #submitted)
      (state .! #addresses)
      block
      finality
      (state .! #utxo)

  finality ∷ Utxo.Finality =
    if thisBlockNo < tipBlockNo - securityParam
      then Utxo.Final
      else Utxo.NotFinal
   where
    BlockNo tipBlockNo = fromWithOrigin 0 (getTipBlockNo tip)
    BlockNo thisBlockNo = blockNo block

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
