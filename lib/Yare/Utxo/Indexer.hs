module Yare.Utxo.Indexer
  ( indexBlock
  , rollbackTo
  , UtxoUpdate (..)
  ) where

import Yare.Prelude

import Cardano.Api (TxIn (..))
import Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)
import Data.List.NonEmpty qualified as NE
import Data.Set (member)
import Fmt.Orphans ()
import NoThunks.Class.Extended (foldlNoThunks)
import Ouroboros.Consensus.Block (blockSlot, pointSlot)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Yare.Address (Addresses, isOwnAddress)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Era (AnyEra (..))
import Yare.Chain.Point (ChainPoint)
import Yare.Chain.Tx
  ( Tx
  , TxId
  , TxOutViewUtxo (..)
  , TxViewUtxo (..)
  , blockTransactions
  , transactionViewUtxo
  )
import Yare.Utxo
  ( Finality (Final, NotFinal)
  , Update (AddSpendableTxInput, SpendTxInput)
  , Utxo
  , finalise
  , rollback
  , spendableTxInputs
  , updateUtxo
  )
import Yare.Utxo qualified as Utxo

data UtxoUpdate
  = -- | The UTxO set which was updated by these transaction IDs
    UtxoUpdated Utxo [AnyEra Tx]
  | UtxoNotUpdated
  | UtxoUpdateError Utxo.UpdateError

{- | Enrich the UTxO set with the transactions from a block.
| Returns the updated UTxO set or 'Nothing' if the block is irrelevant.
-}
indexBlock
  ∷ [TxId]
  -- ^ Submitted transactions
  → Addresses
  -- ^ Addresses of the wallet
  → StdCardanoBlock
  -- ^ The block to index.
  → Finality
  -- ^ Whether the block is final.
  → Utxo
  -- ^ The previous UTxO set.
  → UtxoUpdate
  -- ^ The updated UTxO set or 'Nothing' if the block is irrelevant.
indexBlock submittedTxs addresses block finality prevUtxo =
  case utxoUpdate of
    UtxoNotUpdated → UtxoNotUpdated
    UtxoUpdateError err → UtxoUpdateError err
    UtxoUpdated utxo txIds →
      case finality of
        Final → UtxoUpdated (finalise slot utxo) txIds
        NotFinal → utxoUpdate
 where
  utxoUpdate ∷ UtxoUpdate
  utxoUpdate =
    -- Transaction indexing is implemented as a fold (and not as map/traverse)
    -- such that indexing of a next transaction takes
    -- into account the updates to UTxO made by the previous ones.
    foldlNoThunks
      forEachTx
      (\_utxo → UtxoNotUpdated)
      (blockTransactions block)
      prevUtxo

  forEachTx ∷ (Utxo → UtxoUpdate) → AnyEra Tx → Utxo → UtxoUpdate
  forEachTx prevUpdate !tx utxo =
    case prevUpdate utxo of
      UtxoUpdateError err →
        UtxoUpdateError err
      UtxoNotUpdated →
        case indexTx submittedTxs addresses tx utxo of
          Nothing → UtxoNotUpdated
          Just updates → do
            case updateUtxo slot updates utxo of
              Left updateErr → UtxoUpdateError updateErr
              Right utxo' → UtxoUpdated utxo' (pure tx)
      previous@(UtxoUpdated utxo' txs) →
        case indexTx submittedTxs addresses tx utxo' of
          Nothing → previous
          Just updates →
            case updateUtxo slot updates utxo' of
              Left updateErr → UtxoUpdateError updateErr
              Right utxo'' → UtxoUpdated utxo'' (tx : txs)

  slot ∷ SlotNo
  slot = blockSlot block

{- | Enrich the UTxO set with the information from a transaction.
| Returns the updated UTxO set or 'Nothing' if the transaction is irrelevant.
-}
indexTx
  ∷ [TxId]
  → Addresses
  → AnyEra Tx
  → Utxo
  → Maybe (NonEmpty Update)
indexTx submittedTxs addresses tx utxo = do
  case NE.nonEmpty (indexInsAndOuts (spendableTxInputs utxo) txView) of
    Just updates → Just updates
    Nothing →
      if txId `elem` submittedTxs
        then error $ "Submitted transaction is not indexed: " <> show txView
        else Nothing
 where
  txId = txViewId txView
  txView = transactionViewUtxo tx

  indexInsAndOuts ∷ Set TxIn → TxViewUtxo → [Update]
  indexInsAndOuts spendableInputs TxViewUtxo {..} =
    mapMaybe (indexTxIn spendableInputs) txViewInputs
      <> mapMaybe indexTxOut txViewOutputs

  indexTxIn ∷ Set TxIn → TxIn → Maybe Update
  indexTxIn spendableInputs input =
    guard (input `member` spendableInputs)
      $> SpendTxInput input

  indexTxOut ∷ TxOutViewUtxo → Maybe Update
  indexTxOut TxOutViewUtxo {..} =
    guard (isOwnAddress addresses txOutViewUtxoAddress)
      $> AddSpendableTxInput
        (TxIn txId txOutViewUtxoIndex)
        (force txOutViewUtxoAddress)
        txOutViewUtxoValue

rollbackTo ∷ ChainPoint → Utxo → Maybe Utxo
rollbackTo point = rollback (fromWithOrigin (SlotNo 0) (pointSlot point))
