{-# LANGUAGE TemplateHaskell #-}

module Yare.Utxo -- is meant to be imported qualified
  ( Utxo
  , Entries
  , useByAddress
  , spendableEntries
  , totalValue
  , initial
  , indexBlock
  , rollbackTo
  ) where

import Relude hiding (fromList, show)

import Cardano.Api (TxIn (..), Value, renderTxIn)
import Control.Lens.TH (makeLenses)
import Data.Map.Strict qualified as Map
import Data.Set (member)
import Data.Set qualified as Set
import Fmt (Buildable (..), blockListF, nameF, (+|), (|+))
import Fmt.Orphans ()
import NoThunks.Class.Extended (NoThunks, repeatedly)
import Ouroboros.Consensus.Block (pointSlot)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (blockPoint)
import Yare.Addresses (Addresses, isOwnAddress)
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
import Yare.Chain.Types (LedgerAddress, ledgerAddressToText)

type Entries ∷ Type
type Entries = Map TxIn (LedgerAddress, Value)

type Update ∷ Type
data Update
  = -- | Add a new spendable input to the UTXO set.
    AddSpendableTxInput !TxIn !LedgerAddress Value
  | -- | Remove a spendable input from the UTXO set.
    SpendTxInput !TxIn
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

type Utxo ∷ Type
data Utxo = Utxo
  { reversibleUpdates ∷ ![(ChainPoint, [Update])]
  , finalState ∷ !Entries
  , usedInputs ∷ !(Set TxIn)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks)

instance Buildable Utxo where
  build Utxo {..} =
    "UTXO\n----\n"
      <> nameF "Reversible Updates" do
        blockListF
          [ nameF "Slot" (build (pointSlot chainPoint)) <> blockListF updates
          | (chainPoint, updates) ← reversibleUpdates
          ]
      <> nameF "Final state" do
        blockListF
          [ nameF "Tx Input" (build txIn)
            <> nameF "Address" (build (ledgerAddressToText ledgerAddr))
            <> nameF "Value" (build value)
          | (txIn, (ledgerAddr, value)) ← Map.toList finalState
          ]
      <> nameF "Used inputs" do
        blockListF usedInputs

$(makeLenses ''Utxo)

initial ∷ Utxo
initial =
  Utxo
    { reversibleUpdates = mempty
    , finalState = mempty
    , usedInputs = mempty
    }

{- | Enrich the UTXO set with the transactions from a block.
| Returns the updated UTXO set or 'Nothing' if the block is irrelevant.
-}
indexBlock ∷ Addresses → StdCardanoBlock → Utxo → Maybe Utxo
indexBlock addresses block =
  repeatedly forEachTx (const Nothing) (blockTransactions block)
 where
  forEachTx ∷ (Utxo → Maybe Utxo) → AnyEra Tx → Utxo → Maybe Utxo
  forEachTx !prevUpdate !tx utxo =
    case prevUpdate utxo of
      Nothing → indexTx addresses chainPoint tx utxo
      Just utxo' →
        case indexTx addresses chainPoint tx utxo' of
          Nothing → Just utxo'
          Just utxo'' → Just utxo''

  chainPoint ∷ ChainPoint
  chainPoint = blockPoint block

{- | Enrich the UTXO set with the information from a transaction.
| Returns the updated UTXO set or 'Nothing' if the transaction is irrelevant.
-}
indexTx ∷ Addresses → ChainPoint → AnyEra Tx → Utxo → Maybe Utxo
indexTx addresses point tx utxo =
  updateNonFinalState (reversibleUpdates utxo) <&> \updates →
    utxo {reversibleUpdates = updates}
 where
  updateNonFinalState ∷ [(ChainPoint, [Update])] → Maybe [(ChainPoint, [Update])]
  updateNonFinalState prevUpdates =
    case updateUtxo addresses spendableTxInputs (transactionViewUtxo tx) of
      [] → Nothing
      updates → Just ((point, updates) : prevUpdates)
  spendableTxInputs = Map.keysSet (spendableEntries utxo)

updateUtxo ∷ Addresses → Set TxIn → TxViewUtxo → [Update]
updateUtxo addresses spendableInputs TxViewUtxo {..} =
  mapMaybe (indexTxIn spendableInputs) txViewInputs
    ++ mapMaybe (indexTxOut addresses txViewId) txViewOutputs

indexTxIn ∷ Set TxIn → TxIn → Maybe Update
indexTxIn spendableInputs input =
  guard (input `member` spendableInputs)
    $> SpendTxInput input

indexTxOut ∷ Addresses → TxId → TxOutViewUtxo → Maybe Update
indexTxOut addresses txId TxOutViewUtxo {..} =
  guard (isOwnAddress addresses txOutViewUtxoAddress)
    $> AddSpendableTxInput
      (TxIn txId txOutViewUtxoIndex)
      txOutViewUtxoAddress
      txOutViewUtxoValue

rollbackTo ∷ ChainPoint → Utxo → Maybe Utxo
rollbackTo point utxo = do
  let considerPoint (updatePoint, _update) = updatePoint > point
  case span considerPoint (reversibleUpdates utxo) of
    ([], _remainingUpdates) → Nothing
    (_discardedUpdates, remainingUpdates) →
      Just utxo {reversibleUpdates = remainingUpdates}

--------------------------------------------------------------------------------
-- State updates ---------------------------------------------------------------

useByAddress ∷ Utxo → LedgerAddress → (Utxo, [TxIn])
useByAddress utxo addr = (utxo', inputs)
 where
  inputs = do
    -- This query is not optimized for performance
    (input, (outputAddr, _value)) ← Map.toList (spendableEntries utxo)
    guard $ addr == outputAddr
    pure input

  utxo' = utxo {usedInputs = Set.fromList inputs <> usedInputs utxo}

--------------------------------------------------------------------------------
-- State queries ---------------------------------------------------------------

{- | Returns the spendable entries in the UTXO set.
| Disregards the used inputs.
-}
spendableEntries ∷ Utxo → Entries
spendableEntries utxo = nonFinalSpendableInputs <> finalSpendableInputs
 where
  nonFinalSpendableInputs ∷ Entries
  nonFinalSpendableInputs = foldr overUpdates mempty (reversibleUpdates utxo)
   where
    overUpdates ∷ (ChainPoint, [Update]) → Entries → Entries
    overUpdates (_cp, !updates) = repeatedly overUpdate id updates
     where
      overUpdate ∷ (Entries → Entries) → Update → (Entries → Entries)
      overUpdate !prevUpdate !update =
        case update of
          AddSpendableTxInput input _addr _val
            | input `member` usedInputs utxo →
                prevUpdate
          AddSpendableTxInput input addr val →
            Map.insert input (addr, val) . prevUpdate
          SpendTxInput input →
            Map.delete input . prevUpdate

  finalSpendableInputs ∷ Entries
  finalSpendableInputs = finalState utxo

totalValue ∷ Utxo → Value
totalValue =
  Map.foldr' (\(_addr, value) acc → value <> acc) mempty
    . spendableEntries
