{-# LANGUAGE ImpredicativeTypes #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , ChainState
  , ChainStateᵣ
  , initialChainState
  ) where

import Yare.Prelude

import Cardano.Api.Shelley (TxId)
import Cardano.Slotting.Slot (fromWithOrigin)
import Control.Monad.Class.MonadThrow (throwIO)
import Control.Tracer.Extended (Tracer, traceWith)
import Data.Strict (List)
import Ouroboros.Network.Block
  ( BlockNo (..)
  , Tip (..)
  , blockNo
  , blockPoint
  , blockSlot
  , getTipBlockNo
  )
import Relude.Extra (dup)
import Yare.Address (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip, SyncFrom)
import Yare.Storage (Storage (overStorage))
import Yare.Tracers (Tracersᵣ)
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo
import Yare.Utxo.Indexer (UtxoUpdate (..))
import Yare.Utxo.Indexer qualified as Utxo

data ChainFollower (m ∷ Type → Type) = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

newChainFollower
  ∷ ∀ state env
   . ( (Storage IO state : Addresses : Tracersᵣ) ∈∈ env
     , [Utxo, ChainTip, List TxId, SyncFrom] ∈∈ state
     )
  ⇒ env
  → ChainFollower IO
newChainFollower env =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage (indexBlock (look @Addresses env) block tip) \case
          UtxoNotUpdated →
            traceWith (look env) (blockSlot block)
          UtxoUpdated updatedUtxo txIds → do
            for_ txIds $ traceWith (look env)
            traceWith (look env) updatedUtxo
          UtxoUpdateError err →
            throwIO err
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) → do
        traceWith (look @(Tracer IO ChainPoint) env) point
        overStorage storage (dup . rollbackTo point tip) do
          traceWith (look @(Tracer IO Utxo) env) . look @Utxo
    }
 where
  storage ∷ Storage IO state = look env

type ChainState = HList ChainStateᵣ
type ChainStateᵣ = [Utxo, ChainTip]

initialChainState ∷ ChainState
initialChainState = Utxo.initial .*. TipGenesis .*. HNil

indexBlock
  ∷ ∀ state
   . [Utxo, ChainTip, List TxId, SyncFrom] ∈∈ state
  ⇒ Addresses
  → StdCardanoBlock
  → ChainTip
  → (state → (state, UtxoUpdate))
indexBlock addresses block tip state = (state', utxoUpdate)
 where
  state' ∷ state =
    case utxoUpdate of
      UtxoUpdateError {} → state
      UtxoNotUpdated → state
      UtxoUpdated utxo' _txIds → setter utxo' state
      & setter tip
      & setter (Tagged @"syncFrom" (Just (blockPoint block)))

  utxoUpdate ∷ UtxoUpdate =
    Utxo.indexBlock
      (look @(List TxId) state)
      addresses
      block
      finality
      (look @Utxo state)

  finality ∷ Utxo.Finality =
    if thisBlockNo < tipBlockNo - securityParam
      then Utxo.Final
      else Utxo.NotFinal
   where
    BlockNo tipBlockNo = fromWithOrigin 0 (getTipBlockNo tip)
    BlockNo thisBlockNo = blockNo block

rollbackTo
  ∷ [Utxo, ChainTip, SyncFrom] ∈∈ state
  ⇒ ChainPoint
  → ChainTip
  → state
  → state
rollbackTo point tip chainState =
  chainState
    & setter tip
    & setter (Tagged @"syncFrom" (Just point))
    & maybe id setter (Utxo.rollbackTo point (look @Utxo chainState))

{- | The security parameter is a non-updatable one:
After how many blocks is the blockchain considered to be final,
and thus can no longer be rolled back
(i.e. what is the maximum allowable length of any chain fork).
-}
securityParam ∷ Word64
securityParam = 2160
