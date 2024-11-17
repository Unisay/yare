module Yare.Utxo.Indexer
  ( indexBlock
  , rollbackTo
  , IndexingResult (..)
  ) where

import Yare.Prelude hiding (show)

import Cardano.Api (TxIn (..))
import Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)
import Data.List.NonEmpty qualified as NE
import Data.Set (member)
import Fmt.Orphans ()
import Ouroboros.Consensus.Block (blockSlot, pointSlot)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Text.Show (show)
import Yare.Address (Addresses, isOwnAddress)
import Yare.App.Scripts qualified as Scripts
import Yare.App.Types (Finality (..))
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
  ( Update (..)
  , Utxo
  , finalise
  , rollback
  , spendableTxInputs
  , updateUtxo
  )
import Yare.Utxo qualified as Utxo

data IndexingResult
  = -- | The UTxO set which was updated by these transaction IDs
    UtxoUpdated !Utxo ![AnyEra Tx]
  | UtxoNotUpdated
  | UtxoUpdateError !Utxo.UpdateError

{- | Enrich the UTxO set with the transactions from a block.
| Returns the updated UTxO set or 'Nothing' if the block is irrelevant.
-}
indexBlock
  ∷ Set TxId
  -- ^ Submitted transactions
  → Addresses
  -- ^ Addresses of the wallet
  → StdCardanoBlock
  -- ^ The block to index.
  → Finality
  -- ^ Whether the block is final.
  → Utxo
  -- ^ The previous UTxO set.
  → IndexingResult
  -- ^ The result of indexing the block.
indexBlock submittedTxs addresses block finality prevUtxo =
  case blokIndexingResult of
    UtxoNotUpdated → UtxoNotUpdated
    UtxoUpdateError err → UtxoUpdateError err
    UtxoUpdated utxo txIds →
      case finality of
        Final → UtxoUpdated (finalise slot utxo) txIds
        NotFinal → blokIndexingResult
 where
  blokIndexingResult ∷ IndexingResult
  blokIndexingResult =
    -- Transaction indexing is implemented as a fold (and not as map/traverse)
    -- such that indexing of a next transaction takes
    -- into account the updates to UTxO made by the previous ones.
    foldl' forEachTx UtxoNotUpdated (blockTransactions block)

  forEachTx ∷ IndexingResult → AnyEra Tx → IndexingResult
  forEachTx !previousResult !tx =
    case previousResult of
      UtxoUpdateError {} → previousResult
      UtxoNotUpdated →
        case indexTx submittedTxs addresses tx prevUtxo of
          Nothing → UtxoNotUpdated
          Just updates → do
            case updateUtxo slot updates prevUtxo of
              Left updateErr → UtxoUpdateError updateErr
              Right utxo → UtxoUpdated utxo (pure tx)
      previous@(UtxoUpdated utxo txs) →
        case indexTx submittedTxs addresses tx utxo of
          Nothing → previous
          Just updates →
            case updateUtxo slot updates utxo of
              Left updateErr → UtxoUpdateError updateErr
              Right utxo' → UtxoUpdated utxo' (tx : txs)

  slot ∷ SlotNo
  slot = blockSlot block

{- | Enrich the UTxO set with the information from a transaction.
| Returns the updated UTxO set or 'Nothing' if the transaction is irrelevant.
-}
indexTx
  ∷ Set TxId
  → Addresses
  → AnyEra Tx
  → Utxo
  → Maybe (NonEmpty Update)
indexTx submittedTxs addresses tx utxo = do
  case NE.nonEmpty (indexInsAndOuts (spendableTxInputs utxo) txView) of
    Just updates → Just updates
    Nothing →
      if txId `member` submittedTxs
        then
          error . fromString $
            "Submitted transaction wasn't indexed: " <> show txView
        else Nothing
 where
  txId = txViewId txView
  txView = transactionViewUtxo tx

  indexInsAndOuts ∷ Set TxIn → TxViewUtxo → [Update]
  indexInsAndOuts spendableInputs TxViewUtxo {..} =
    mapMaybe (indexTxIn spendableInputs) txViewInputs
      <> (indexTxOut =<< txViewOutputs)

  indexTxIn ∷ Set TxIn → TxIn → Maybe Update
  indexTxIn spendableInputs input =
    guard (input `member` spendableInputs)
      $> SpendTxInput input

  indexTxOut ∷ TxOutViewUtxo → [Update]
  indexTxOut txOut =
    maybeToList (checkTxOutAddress txOut)
      ++ maybeToList (checkTxOutScript txOut)

  checkTxOutAddress ∷ TxOutViewUtxo → Maybe Update
  checkTxOutAddress TxOutViewUtxo {..} = do
    guard $ isOwnAddress addresses txOutViewUtxoAddress
    pure $
      AddSpendableTxInput
        (TxIn txId txOutViewUtxoIndex)
        (force txOutViewUtxoAddress)
        txOutViewUtxoValue

  checkTxOutScript ∷ TxOutViewUtxo → Maybe Update
  checkTxOutScript out = do
    scriptHash ← Scripts.isOwnScriptAddress (txOutViewUtxoAddress out)
    let txIn = TxIn txId (txOutViewUtxoIndex out)
    pure $ ConfirmScriptDeployment scriptHash txIn

rollbackTo ∷ ChainPoint → Utxo → Maybe Utxo
rollbackTo point = rollback (fromWithOrigin (SlotNo 0) (pointSlot point))
