module Yare.Utxo.State
  ( UtxoState
  , indexBlock
  , rollbackTo
  , initialState
  , spendableUtxoEntries
  ) where

import Relude

-- These 3 imports are required as they bring required instances into scope.
import Ouroboros.Consensus.Cardano.Node ()

-- import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

import Cardano.Api (TxIn (..), Value)
import Data.Map.Strict qualified as Map
import Data.Set (member)
import Ouroboros.Network.Block (blockPoint)
import Yare.Addresses (Addresses, isOwnAddress)
import Yare.Chain.Block (HFBlock)
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
import Yare.Chain.Types (LedgerAddress)
import Yare.Utxo (Utxo, utxoEntries)

type UtxoState ∷ Type
data UtxoState = UtxoState
  { nonFinalState ∷ [(ChainPoint, [UtxoUpdate])]
  , finalState ∷ Utxo
  }
  deriving stock (Eq, Show)

initialState ∷ UtxoState
initialState = UtxoState {nonFinalState = mempty, finalState = mempty}

indexBlock ∷ Addresses → HFBlock → UtxoState → UtxoState
indexBlock addresses block s = s'
 where
  s' = foldr forEachTx id (blockTransactions block) s
  forEachTx tx nextUpdate = nextUpdate . indexTx addresses chainPoint tx
  chainPoint = blockPoint block

indexTx ∷ Addresses → ChainPoint → AnyEra Tx → UtxoState → UtxoState
indexTx addresses point tx s =
  s {nonFinalState = updateNonFinalState (nonFinalState s)}
 where
  spendableTxInputs = Map.keysSet (spendableUtxoEntries s)
  updateNonFinalState =
    updateUtxo addresses spendableTxInputs (transactionViewUtxo tx) & \case
      [] → id
      updates → ((point, updates) :)

updateUtxo ∷ Addresses → Set TxIn → TxViewUtxo → [UtxoUpdate]
updateUtxo addresses spendableInputs TxViewUtxo {..} =
  mapMaybe (indexTxIn spendableInputs) txViewInputs
    ++ mapMaybe (indexTxOut addresses txViewId) txViewOutputs

indexTxIn ∷ Set TxIn → TxIn → Maybe UtxoUpdate
indexTxIn spendableInputs input =
  guard (input `member` spendableInputs)
    $> SpendTxInput input

indexTxOut ∷ Addresses → TxId → TxOutViewUtxo → Maybe UtxoUpdate
indexTxOut addresses txId TxOutViewUtxo {..} =
  guard (isOwnAddress addresses txOutViewUtxoAddress)
    $> AddSpendableTxInput
      (TxIn txId txOutViewUtxoIndex)
      txOutViewUtxoAddress
      txOutViewUtxoValue

rollbackTo ∷ ChainPoint → UtxoState → UtxoState
rollbackTo point s =
  s {nonFinalState = dropWhile ((> point) . fst) (nonFinalState s)}

type UtxoUpdate ∷ Type
data UtxoUpdate
  = AddSpendableTxInput TxIn LedgerAddress Value
  | SpendTxInput TxIn
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- State queries ---------------------------------------------------------------

spendableUtxoEntries ∷ UtxoState → Map TxIn (LedgerAddress, Value)
spendableUtxoEntries s = nonFinalSpendableInputs <> finalSpendableInputs
 where
  nonFinalSpendableInputs ∷ Map TxIn (LedgerAddress, Value)
  nonFinalSpendableInputs = foldr overUpdates mempty (nonFinalState s)
   where
    overUpdates
      ∷ (ChainPoint, [UtxoUpdate])
      → Map TxIn (LedgerAddress, Value)
      → Map TxIn (LedgerAddress, Value)
    overUpdates (_cp, updates) = foldr overUpdate id updates
     where
      overUpdate
        ∷ UtxoUpdate
        → (Map TxIn (LedgerAddress, Value) → Map TxIn (LedgerAddress, Value))
        → (Map TxIn (LedgerAddress, Value) → Map TxIn (LedgerAddress, Value))
      overUpdate update nextUpdate =
        case update of
          AddSpendableTxInput input addr val →
            nextUpdate . Map.insert input (addr, val)
          SpendTxInput input →
            nextUpdate . Map.delete input

  finalSpendableInputs ∷ Map TxIn (LedgerAddress, Value)
  finalSpendableInputs = utxoEntries (finalState s)
