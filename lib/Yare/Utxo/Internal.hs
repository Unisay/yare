module Yare.Utxo.Internal where

import Yare.Prelude

import Cardano.Api (TxIn (..), Value, renderTxIn)
import Cardano.Api.Orphans ()
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.Serialise (Serialise)
import Codec.Serialise.Class.Orphans ()
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
    ConfirmScriptDeployment !TxIn
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
    ConfirmScriptDeployment input →
      "ConfirmScriptDeployment " +| renderTxIn input |+ ""

data UpdateError
  = NoTxInputToSpend TxIn
  | InvalidScriptDeploymentConfirmation ScriptDeployment TxIn
  | InputAlreadyExists TxIn
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Exception)

data ScriptDeployment = NotInitiated | Submitted !TxIn | Deployed !TxIn
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks, Serialise)

data Utxo = Utxo
  { reversibleUpdates ∷ ![(SlotNo, NonEmpty Update)]
  , finalEntries ∷ !Entries
  , usedInputs ∷ !(Set TxIn)
  , scriptDeployment ∷ !ScriptDeployment
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks, Serialise)

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

data Finality = Final | NotFinal
  deriving stock (Eq, Show, Generic, Enum, Bounded)

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
  fmap head . traverse \case
    AddSpendableTxInput txIn _addr _value →
      guard (txIn `member` Map.keysSet (allEntries utxo))
        $> InputAlreadyExists txIn
    SpendTxInput txIn →
      guard (txIn `notMember` Map.keysSet (allEntries utxo))
        $> NoTxInputToSpend txIn
    ConfirmScriptDeployment txIn →
      let prev = scriptDeployment utxo
       in case prev of
            NotInitiated →
              pure $ InvalidScriptDeploymentConfirmation prev txIn
            Deployed _txIn →
              pure $ InvalidScriptDeploymentConfirmation prev txIn
            Submitted submittedTxIn →
              guard (txIn /= submittedTxIn)
                $> InvalidScriptDeploymentConfirmation prev txIn

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

-- | Finalize the UTxO set up to the given slot.
finalise ∷ SlotNo → Utxo → Utxo
finalise
  immutableSlot
  utxo@Utxo
    { scriptDeployment
    , reversibleUpdates
    , finalEntries
    } =
    utxo
      { reversibleUpdates = reversibleUpdates'
      , finalEntries = updatesToEntries irreversibleUpdates <> finalEntries
      , scriptDeployment =
          case scriptDeployment of
            NotInitiated → NotInitiated
            Deployed txIn → Deployed txIn
            Submitted expectedInput →
              fromMaybe scriptDeployment $
                listToMaybe
                  [ Deployed input
                  | (_slot, slotUpdates) ← irreversibleUpdates
                  , ConfirmScriptDeployment input ← toList slotUpdates
                  , input == expectedInput
                  ]
      }
   where
    (reversibleUpdates', irreversibleUpdates) =
      span (fst >>> (> immutableSlot)) reversibleUpdates

setScriptDeployment ∷ ScriptDeployment → Utxo → Utxo
setScriptDeployment scriptDeployment utxo = utxo {scriptDeployment}

--------------------------------------------------------------------------------
-- UTxO queries ----------------------------------------------------------------

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

txInputs ∷ Utxo → Set TxIn
txInputs = Map.keysSet . allEntries

spendableTxInputs ∷ Utxo → Set TxIn
spendableTxInputs = Map.keysSet . spendableEntries

totalValue ∷ Utxo → Value
totalValue =
  Map.foldr' (\(_addr, value) acc → value <> acc) mempty . spendableEntries

-- | Converts a list of UTxO updates to a map of UTxO entries.
updatesToEntries ∷ [(SlotNo, NonEmpty Update)] → Entries
updatesToEntries = foldr overUpdates mempty
 where
  overUpdates ∷ (SlotNo, NonEmpty Update) → Entries → Entries
  overUpdates (_slot, !updates) = foldlNoThunks overUpdate id (toList updates)
   where
    overUpdate ∷ (Entries → Entries) → Update → (Entries → Entries)
    overUpdate !prevUpdate = \case
      ConfirmScriptDeployment _input → prevUpdate
      SpendTxInput input → Map.delete input . prevUpdate
      AddSpendableTxInput input addr val →
        Map.insert input (addr, val) . prevUpdate
