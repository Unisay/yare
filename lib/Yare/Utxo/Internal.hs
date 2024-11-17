module Yare.Utxo.Internal where

import Yare.Prelude

import Cardano.Api (ScriptHash, TxIn (..), Value, renderTxIn)
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.Serialise (Serialise)
import Codec.Serialise.Class.Orphans ()
import Control.DeepSeq.Orphans ()
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (member, notMember)
import Data.Set qualified as Set
import Fmt (Buildable (..), Builder, blockListF, nameF, (+|), (|+))
import Fmt.Orphans ()
import NoThunks.Class.Extended (NoThunks (..), foldlNoThunks)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Yare.Address (AddressWithKey (..), Addresses)
import Yare.Address qualified as Addresses
import Yare.Chain.Types (LedgerAddress, ledgerAddressToText)

--------------------------------------------------------------------------------
-- Data Types ------------------------------------------------------------------

type Entries = Map TxIn (LedgerAddress, Value)

data Update
  = -- | Add a new spendable input to the UTxO set.
    AddSpendableTxInput !TxIn !LedgerAddress !Value
  | -- | Remove a spendable input from the UTxO set.
    SpendTxInput !TxIn
  | -- | Confirm that previously submitted script deployment tx
    --  is in the blockchain.
    ConfirmScriptDeployment !ScriptHash !TxIn
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks, Serialise)

instance Buildable Update where
  build = \case
    AddSpendableTxInput input addr val →
      nameF "AddSpendableTxInput" do
        nameF "Input" (build (renderTxIn input))
          <> nameF "Address" (build (ledgerAddressToText addr))
          <> nameF "Value" (build val)
    SpendTxInput input →
      "SpendTxInput " +| renderTxIn input |+ ""
    ConfirmScriptDeployment scriptHash input →
      "ConfirmScriptDeployment " +| scriptHash |+ " " +| renderTxIn input |+ ""

data UpdateError
  = NoTxInputToSpend TxIn
  | InvalidScriptDeploymentConfirmation (Maybe ScriptStatus) TxIn
  | InputAlreadyExists TxIn
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Exception)

data ScriptStatus
  = ScriptStatusDeployInitiated
  | ScriptStatusDeployCompleted
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving anyclass (NoThunks, NFData, Serialise)

instance Buildable ScriptStatus where
  build =
    nameF "Status" . \case
      ScriptStatusDeployInitiated → "Initiated"
      ScriptStatusDeployCompleted → "Completed"

data ScriptDeployment = ScriptDeployment
  { scriptTxIn ∷ !TxIn
  , scriptStatus ∷ !ScriptStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, NFData, Serialise)

instance Buildable ScriptDeployment where
  build ScriptDeployment {scriptTxIn, scriptStatus} =
    nameF "Script Deployment" $
      nameF "TxIn" (build (renderTxIn scriptTxIn)) <> build scriptStatus

data Utxo = Utxo
  { reversibleUpdates ∷ ![(SlotNo, NonEmpty Update)]
  , finalEntries ∷ !Entries
  , usedInputs ∷ !(Set TxIn)
  , scriptDeployments ∷ !(Map ScriptHash ScriptDeployment)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks, Serialise)

instance Buildable Utxo where
  build utxo@Utxo {..} =
    nameF "Reversible UTxO updates" do
      blockListF
        [ nameF "SlotNo" (build slotNo) <> blockListF updates
        | (slotNo, updates) ← reversibleUpdates
        ]
      <> nameF "Final UTxO entries" do
        blockListF
          [ buildTxIn txIn addr value
          | (txIn, (addr, value)) ← Map.toList finalEntries
          ]
      <> nameF "Inputs used by submitted transactions" (blockListF usedInputs)
      <> nameF
        "Script deployments"
        ( blockListF
            [ build scriptHash <> build scriptDeployment
            | (scriptHash, scriptDeployment) ← Map.toList scriptDeployments
            ]
        )
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
    , scriptDeployments = mempty
    }

--------------------------------------------------------------------------------
-- UTxO updates ----------------------------------------------------------------

useFeeInputs
  ∷ ∀ state
   . Utxo ∈ state
  ⇒ Addresses
  → state
  → Maybe (state, NonEmpty (TxIn, (AddressWithKey, Value)))
useFeeInputs addresses s = do
  feeInputs ← NE.nonEmpty (first (const feeAddress) <<$>> feeEntries)
  pure (s', feeInputs)
 where
  s' = setter utxo' s
  feeAddress = Addresses.useForFees addresses
  (utxo', feeEntries) = useByAddress utxo (ledgerAddress feeAddress)
  utxo ∷ Utxo = look s

useCollateralInputs
  ∷ ∀ state
   . Utxo ∈ state
  ⇒ Addresses
  → state
  → Maybe (state, NonEmpty (TxIn, (AddressWithKey, Value)))
useCollateralInputs addresses s = do
  colInputs ← NE.nonEmpty collateralEntries
  pure (s', first (const collateralAddressWithKey) <<$>> colInputs)
 where
  s' = setter utxo' s
  collateralAddressWithKey = Addresses.useForCollateral addresses
  (utxo', collateralEntries) = useByAddress utxo collateralAddr
  collateralAddr = ledgerAddress collateralAddressWithKey
  utxo ∷ Utxo = look s

updateUtxo ∷ SlotNo → NonEmpty Update → Utxo → Either UpdateError Utxo
updateUtxo slot updates utxo =
  utxo {reversibleUpdates = (slot, updates) : reversibleUpdates utxo}
    `maybeToLeft` validateUpdates utxo updates

validateUpdates ∷ Utxo → NonEmpty Update → Maybe UpdateError
validateUpdates utxo =
  asum . fmap \case
    AddSpendableTxInput txIn _addr _value →
      guard (txIn `member` Map.keysSet (allEntries utxo))
        $> InputAlreadyExists txIn
    SpendTxInput txIn →
      guard (txIn `notMember` Map.keysSet (allEntries utxo))
        $> NoTxInputToSpend txIn
    ConfirmScriptDeployment scriptHash confirmedTxIn →
      case Map.lookup scriptHash (scriptDeployments utxo) of
        Nothing →
          Just $ InvalidScriptDeploymentConfirmation Nothing confirmedTxIn
        Just (ScriptDeployment initiatedTxIn scriptStatus) →
          case scriptStatus of
            ScriptStatusDeployCompleted →
              Just $
                InvalidScriptDeploymentConfirmation
                  (Just scriptStatus)
                  confirmedTxIn
            ScriptStatusDeployInitiated →
              guard (confirmedTxIn /= initiatedTxIn)
                $> InvalidScriptDeploymentConfirmation
                  (Just scriptStatus)
                  confirmedTxIn

rollback ∷ SlotNo → Utxo → Maybe Utxo
rollback rollbackSlot utxo =
  case span (fst >>> (> rollbackSlot)) (reversibleUpdates utxo) of
    ([], _remainingUpdates) → Nothing
    (_discardedUpdates, remainingUpdates) →
      Just utxo {reversibleUpdates = remainingUpdates}

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

-- | Finalize the UTxO set up to the given slot (inclusive).
finalise ∷ SlotNo → Utxo → Utxo
finalise
  immutableSlot
  utxo@Utxo
    { scriptDeployments
    , reversibleUpdates
    , finalEntries
    } =
    utxo
      { reversibleUpdates = reversibleUpdates'
      , finalEntries = updatesToEntries irreversibleUpdates finalEntries
      , scriptDeployments =
          let
            confirmedScriptDeployments ∷ [(ScriptHash, TxIn)] = do
              (_slot, slotUpdates) ← irreversibleUpdates
              ConfirmScriptDeployment hash input ← toList slotUpdates
              pure (hash, input)

            applyConfirmation
              ∷ Map ScriptHash ScriptDeployment
              → (ScriptHash, TxIn)
              → Map ScriptHash ScriptDeployment
            applyConfirmation !statuses (!hash, !txIn) =
              let deployment = ScriptDeployment txIn ScriptStatusDeployCompleted
               in Map.insert hash deployment statuses
           in
            foldl'
              applyConfirmation
              scriptDeployments
              confirmedScriptDeployments
      }
   where
    (reversibleUpdates', irreversibleUpdates) =
      span (fst >>> (> immutableSlot)) reversibleUpdates

initiateScriptDeployment ∷ ScriptHash → TxIn → Utxo → Utxo
initiateScriptDeployment hash input utxo =
  utxo {scriptDeployments = Map.insert hash deployment (scriptDeployments utxo)}
 where
  deployment = ScriptDeployment input ScriptStatusDeployInitiated

--------------------------------------------------------------------------------
-- UTxO queries ----------------------------------------------------------------

-- | All entries in the UTxO set: both reversible and final inputs.
allEntries ∷ Utxo → Entries
allEntries Utxo {reversibleUpdates, finalEntries} =
  updatesToEntries reversibleUpdates finalEntries

{- | Spendable entries in the UTxO set:
Everything that could be spent (i.e. not already used),
including both reversible and final inputs.
-}
spendableEntries ∷ Utxo → Entries
spendableEntries utxo = Map.withoutKeys (allEntries utxo) (usedInputs utxo)

txInputs ∷ Utxo → Set TxIn
txInputs = Map.keysSet . allEntries

spendableTxInputs ∷ Utxo → Set TxIn
spendableTxInputs = Map.keysSet . spendableEntries

totalValue ∷ Utxo → Value
totalValue =
  Map.foldr' (\(_addr, value) acc → value <> acc) mempty . spendableEntries

-- | Converts a list of UTxO updates to a map of UTxO entries.
updatesToEntries ∷ [(SlotNo, NonEmpty Update)] → Entries → Entries
updatesToEntries allUpdates entries = foldr overUpdates entries allUpdates
 where
  overUpdates ∷ (SlotNo, NonEmpty Update) → Entries → Entries
  overUpdates (_slot, !updates) = foldlNoThunks overUpdate id (toList updates)
   where
    overUpdate ∷ (Entries → Entries) → Update → (Entries → Entries)
    overUpdate !prevUpdate = \case
      ConfirmScriptDeployment _hash _input → prevUpdate
      SpendTxInput input → Map.delete input . prevUpdate
      AddSpendableTxInput input addr val →
        Map.insert input (addr, val) . prevUpdate
