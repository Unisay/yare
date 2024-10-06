module Yare.Utxo -- is meant to be imported qualified
  ( Utxo
  , UtxoUpdate (..)
  , ScriptDeployment (..)
  , Entries

    -- * Updtes
  , indexBlock
  , rollbackTo
  , useByAddress
  , setScriptDeployment
  , finalise

    -- * Queries
  , initial
  , spendableEntries
  , totalValue
  ) where

import Yare.Prelude

import Cardano.Api (TxIn (..), Value, renderTxIn)
import Cardano.Slotting.Slot (SlotNo (..), fromWithOrigin)
import Data.DList (DList)
import Data.Map.Strict qualified as Map
import Data.Set (member)
import Data.Set qualified as Set
import Data.Strict (List)
import Data.Tagged (Tagged (unTagged))
import Fmt (Buildable (..), Builder, blockListF, nameF, (+|), (|+))
import Fmt.Orphans ()
import NoThunks.Class.Extended (NoThunks (..), repeatedly)
import Ouroboros.Consensus.Block (blockSlot, pointSlot)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Yare.Address (Addresses, isOwnAddress)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Era (AnyEra (..))
import Yare.Chain.Tx
  ( Tx
  , TxId
  , TxOutViewUtxo (..)
  , TxViewUtxo (..)
  , blockTransactions
  , transactionViewUtxo
  )
import Yare.Chain.Types (ChainPoint, LedgerAddress, ledgerAddressToText)

type Entries = Map TxIn (LedgerAddress, Value)

data Update
  = -- | Add a new spendable input to the UTxO set.
    AddSpendableTxInput !TxIn !LedgerAddress !Value
  | -- | Remove a spendable input from the UTxO set.
    SpendTxInput !TxIn
  | -- | Confirm that previously submitted script deployment tx
    --  is in the blockchain.
    ConfirmScriptDeployment !TxIn
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks)

instance Buildable Update where
  build = \case
    AddSpendableTxInput input addr val →
      nameF "AddSpendableTxInput" do
        nameF "Input" (build (renderTxIn input))
          <> nameF "Address" (build (ledgerAddressToText addr))
          <> nameF "Value" (build val)
    SpendTxInput input →
      "SpendTxInput " +| renderTxIn input |+ ""
    ConfirmScriptDeployment input →
      "ConfirmScriptDeployment " +| renderTxIn input |+ ""

data ScriptDeployment = NotInitiated | Submitted !TxIn | Deployed !TxIn
  deriving stock (Show, Generic)
  deriving anyclass (NoThunks)

data Utxo = Utxo
  { reversibleUpdates ∷ ![(SlotNo, [Update])]
  , finalEntries ∷ !Entries
  , usedInputs ∷ !(Set TxIn)
  , scriptDeployment ∷ !ScriptDeployment
  }
  deriving stock (Show, Generic)
  deriving anyclass (NoThunks)

instance Buildable Utxo where
  build utxo@Utxo {..} =
    nameF "Reversible UTxO updates" do
      blockListF
        [ nameF "BlockNo" (build block) <> blockListF updates
        | (block, updates) ← reversibleUpdates
        ]
      <> nameF "Final UTxO entries" do
        blockListF
          [ buildTxIn txIn addr value
          | (txIn, (addr, value)) ← Map.toList finalEntries
          ]
      <> nameF "Inputs used by submitted transactions" (blockListF usedInputs)
      <> if null reversibleUpdates
        then "\n"
        else nameF "Applied UTxO updates" do
          blockListF
            [ buildTxIn txIn addr value
            | (txIn, (addr, value)) ← Map.toList (spendableEntries utxo)
            ]
   where
    buildTxIn ∷ TxIn → LedgerAddress → Value → Builder
    buildTxIn txIn ledgerAddr value =
      fold
        [ nameF "Tx Input" (build txIn)
        , nameF "Address" (build (ledgerAddressToText ledgerAddr))
        , nameF "Value" (build value)
        ]

initial ∷ Utxo
initial =
  Utxo
    { reversibleUpdates = mempty
    , finalEntries = mempty
    , usedInputs = mempty
    , scriptDeployment = NotInitiated
    }

data UtxoUpdate
  = -- | The UTxO set which was updated by these transaction IDs
    UtxoUpdated Utxo (DList TxId)
  | UtxoNotUpdated

mapUpdate ∷ (Utxo → Utxo) → (DList TxId → DList TxId) → UtxoUpdate → UtxoUpdate
mapUpdate f g = \case
  UtxoUpdated utxo txIds → UtxoUpdated (f utxo) (g txIds)
  UtxoNotUpdated → UtxoNotUpdated

{- | Enrich the UTxO set with the transactions from a block.
| Returns the updated UTxO set or 'Nothing' if the block is irrelevant.
-}
indexBlock
  ∷ List TxId
  -- ^ Submitted transactions
  → Addresses
  -- ^ Addresses of the wallet
  → StdCardanoBlock
  -- ^ The block to index.
  → Tagged "isFinal" Bool
  -- ^ Whether the block is final.
  → Utxo
  -- ^ The previous UTxO set.
  → UtxoUpdate
  -- ^ The updated UTxO set or 'Nothing' if the block is irrelevant.
indexBlock submittedTxs addresses block isFinal prevUtxo =
  repeatedly forEachTx (const UtxoNotUpdated) (blockTransactions block) prevUtxo
    & if unTagged isFinal then mapUpdate (finalise slot) id else id
 where
  slot ∷ SlotNo
  slot = blockSlot block

  forEachTx ∷ (Utxo → UtxoUpdate) → AnyEra Tx → Utxo → UtxoUpdate
  forEachTx !prevUpdate !tx utxo =
    case prevUpdate utxo of
      UtxoNotUpdated → indexTx submittedTxs addresses slot tx utxo
      previous@(UtxoUpdated utxo' txIds) →
        case indexTx submittedTxs addresses slot tx utxo' of
          UtxoNotUpdated → previous
          UtxoUpdated utxo'' txIds' → UtxoUpdated utxo'' (txIds <> txIds')

{- | Enrich the UTxO set with the information from a transaction.
| Returns the updated UTxO set or 'Nothing' if the transaction is irrelevant.
-}
indexTx ∷ List TxId → Addresses → SlotNo → AnyEra Tx → Utxo → UtxoUpdate
indexTx submittedTxs addresses point tx utxo = do
  let txView = transactionViewUtxo tx
      txId = txViewId txView
  case updateNonFinalState (reversibleUpdates utxo) of
    Just updates →
      UtxoUpdated utxo {reversibleUpdates = updates} (pure txId)
    Nothing →
      if txViewId txView `elem` submittedTxs
        then error $ "Submitted transaction is not indexed: " <> show txView
        else UtxoNotUpdated
 where
  updateNonFinalState
    ∷ [(SlotNo, [Update])]
    → Maybe [(SlotNo, [Update])]
  updateNonFinalState prevUpdates =
    case updateUtxo spendableTxInputs (transactionViewUtxo tx) of
      [] → Nothing
      updates → Just ((point, updates) : prevUpdates)
  spendableTxInputs = Map.keysSet (spendableEntries utxo)

  updateUtxo ∷ Set TxIn → TxViewUtxo → [Update]
  updateUtxo spendableInputs TxViewUtxo {..} =
    mapMaybe (indexTxIn spendableInputs) txViewInputs
      ++ mapMaybe (indexTxOut txViewId) txViewOutputs

  indexTxIn ∷ Set TxIn → TxIn → Maybe Update
  indexTxIn spendableInputs input =
    guard (input `member` spendableInputs)
      $> SpendTxInput input

  indexTxOut ∷ TxId → TxOutViewUtxo → Maybe Update
  indexTxOut txId TxOutViewUtxo {..} =
    guard (isOwnAddress addresses txOutViewUtxoAddress)
      $> AddSpendableTxInput
        (TxIn txId txOutViewUtxoIndex)
        (force txOutViewUtxoAddress)
        txOutViewUtxoValue

rollbackTo ∷ ChainPoint → Utxo → Maybe Utxo
rollbackTo (fromWithOrigin (SlotNo 0) . pointSlot → rollbackSlot) utxo = do
  case span (fst >>> (> rollbackSlot)) (reversibleUpdates utxo) of
    ([], _remainingUpdates) → Nothing
    (_discardedUpdates, remainingUpdates) →
      Just utxo {reversibleUpdates = remainingUpdates}

--------------------------------------------------------------------------------
-- State updates ---------------------------------------------------------------

useByAddress ∷ Utxo → LedgerAddress → (Utxo, [(TxIn, (LedgerAddress, Value))])
useByAddress utxo addr = (utxo', entries)
 where
  utxo' = utxo {usedInputs = Set.fromList inputs <> usedInputs utxo}
  inputs = map fst entries
  entries = do
    -- This query is not optimized for performance
    entry@(_input, (outputAddr, _value)) ← Map.toList (spendableEntries utxo)
    guard $ addr == outputAddr
    pure entry

-- | Finalize the UTxO set up to the given slot.
finalise ∷ SlotNo → Utxo → Utxo
finalise immutableSlot utxo@Utxo {scriptDeployment, reversibleUpdates} =
  utxo
    { reversibleUpdates = reversibleUpdates'
    , finalEntries = updatesToEntries irreversibleUpdates
    , scriptDeployment =
        case scriptDeployment of
          NotInitiated → NotInitiated
          Deployed txIn → Deployed txIn
          Submitted expectedInput →
            fromMaybe scriptDeployment $
              listToMaybe
                [ Deployed input
                | (_point, pointUpdates) ← irreversibleUpdates
                , ConfirmScriptDeployment input ← pointUpdates
                , input == expectedInput
                ]
    }
 where
  (reversibleUpdates', irreversibleUpdates) =
    span (fst >>> (> immutableSlot)) reversibleUpdates

setScriptDeployment ∷ ScriptDeployment → Utxo → Utxo
setScriptDeployment scriptDeployment utxo = utxo {scriptDeployment}

--------------------------------------------------------------------------------
-- State queries ---------------------------------------------------------------

-- | All entries in the UTxO set: both reversible and final inputs.
allEntries ∷ Utxo → Entries
allEntries Utxo {reversibleUpdates, finalEntries} =
  updatesToEntries reversibleUpdates <> finalEntries

{- | Spendable entries in the UTxO set:
Everything that could be spent (i.e. not already used),
including both reversible and final inputs.
-}
spendableEntries ∷ Utxo → Entries
spendableEntries utxo = Map.withoutKeys (allEntries utxo) (usedInputs utxo)

totalValue ∷ Utxo → Value
totalValue =
  Map.foldr' (\(_addr, value) acc → value <> acc) mempty . spendableEntries

-- | Converts a list of UTxO updates to a map of UTxO entries.
updatesToEntries ∷ [(SlotNo, [Update])] → Entries
updatesToEntries = foldr overUpdates mempty
 where
  overUpdates ∷ (SlotNo, [Update]) → Entries → Entries
  overUpdates (_cp, !updates) = repeatedly overUpdate id updates
   where
    overUpdate ∷ (Entries → Entries) → Update → (Entries → Entries)
    overUpdate !prevUpdate = \case
      ConfirmScriptDeployment _input → prevUpdate
      SpendTxInput input → Map.delete input . prevUpdate
      AddSpendableTxInput input addr val →
        Map.insert input (addr, val) . prevUpdate
