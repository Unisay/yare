module Yare.Utxo.Internal where

import Yare.Prelude hiding (update)

import Cardano.Api.Shelley
  ( AssetId (AdaAssetId)
  , ScriptHash
  , TxIn (..)
  , Value
  , renderTxIn
  , selectLovelace
  )
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Slotting.Slot (SlotNo (..))
import Codec.Serialise (Serialise)
import Codec.Serialise.Class.Orphans ()
import Control.DeepSeq.Orphans ()
import Data.Foldable (foldrM)
import Data.List.NonEmpty ((<|))
import Data.Map.Strict qualified as Map
import Data.Set (member, notMember)
import Data.Set qualified as Set
import Fmt (Buildable (..), Builder, blockListF, nameF, (+|), (|+))
import Fmt.Orphans ()
import GHC.IsList qualified as GHC
import NoThunks.Class.Extended (NoThunks (..))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Yare.Address (AddressWithKey (..), Addresses)
import Yare.Address qualified as Addresses
import Yare.Chain.Types (LedgerAddress, ledgerAddressToText)

--------------------------------------------------------------------------------
-- Data Types ------------------------------------------------------------------

data Entry = MkEntry
  { utxoEntryInput ∷ !TxIn
  , utxoEntryAddress ∷ !LedgerAddress
  , utxoEntryValue ∷ !Value
  , utxoEntryKey ∷ Crypto.XPrv
  }
  deriving stock (Generic)
  deriving anyclass (NFData, NoThunks)

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
  | ScriptDeploymentConfirmationDuplicate (Maybe ScriptStatus) TxIn
  | ScriptDeploymentConfirmationMismatch (Maybe ScriptStatus) TxIn
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
  , finalEntries ∷ !(Map TxIn (LedgerAddress, Value))
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

useInputFee ∷ Addresses → Utxo → Maybe (Utxo, Entry)
useInputFee = flip useInputWithAddress . Addresses.useForFee

useInputCollateral ∷ Addresses → Utxo → Maybe (Utxo, Entry)
useInputCollateral = flip useInputWithAddress . Addresses.useForCollateral

useInputWithAddress ∷ Utxo → AddressWithKey → Maybe (Utxo, Entry)
useInputWithAddress utxo MkAddressWithKey {..} = do
  let (utxo', usedByAddress) = useByAddress utxo ledgerAddress
  entry@MkEntry {utxoEntryInput} ←
    entryWithLowestValue do
      (utxoEntryInput, (addr, utxoEntryValue)) ← usedByAddress
      guard (addr == ledgerAddress)
        $> MkEntry
          { utxoEntryInput
          , utxoEntryValue
          , utxoEntryKey = paymentKey
          , utxoEntryAddress = ledgerAddress
          }
  let utxo'' = utxo' {usedInputs = Set.insert utxoEntryInput (usedInputs utxo')}
  pure (utxo'', entry)

useInputLowestAdaOnly ∷ HasCallStack ⇒ Addresses → Utxo → Maybe (Utxo, Entry)
useInputLowestAdaOnly addresses utxo = do
  entry@MkEntry {utxoEntryInput} ← entryWithLowestValue do
    (input, (addr, value)) ← Map.toList (spendableEntries utxo)
    pure case Addresses.asOwnAddress addresses addr of
      Nothing → impossible "UTxO entry address is not own"
      Just MkAddressWithKey {..} →
        MkEntry
          { utxoEntryInput = input
          , utxoEntryValue = value
          , utxoEntryKey = paymentKey
          , utxoEntryAddress = ledgerAddress
          }
  pure (utxo {usedInputs = Set.insert utxoEntryInput (usedInputs utxo)}, entry)

entryWithLowestValue ∷ [Entry] → Maybe Entry
entryWithLowestValue entries =
  viaNonEmpty head $ sortOn (selectLovelace . utxoEntryValue) do
    entry@MkEntry {utxoEntryValue} ← entries
    let assets = [asset | (asset, _qty) ← GHC.toList utxoEntryValue]
    guard $ all (== AdaAssetId) assets
    pure entry

updateUtxo ∷ SlotNo → NonEmpty Update → Utxo → Either UpdateError Utxo
updateUtxo slot updates utxo = foldrM (applyUpdate slot) utxo updates

applyUpdate ∷ SlotNo → Update → Utxo → Either UpdateError Utxo
applyUpdate slot update utxo@Utxo {reversibleUpdates} = do
  case validateUpdate utxo update of
    Just err → Left err
    Nothing →
      Right case reversibleUpdates of
        [] → utxo {reversibleUpdates = [(slot, pure update)]}
        (prevSlot, prevUpdates) : rest
          | prevSlot == slot →
              utxo {reversibleUpdates = (slot, update <| prevUpdates) : rest}
        _ → utxo {reversibleUpdates = (slot, pure update) : reversibleUpdates}

validateUpdate ∷ Utxo → Update → Maybe UpdateError
validateUpdate utxo = \case
  AddSpendableTxInput txIn _addr _value →
    guard (txIn `member` txInputs utxo) $> InputAlreadyExists txIn
  SpendTxInput txIn →
    guard (txIn `notMember` txInputs utxo) $> NoTxInputToSpend txIn
  ConfirmScriptDeployment scriptHash txIn' →
    Map.lookup scriptHash (scriptDeployments utxo)
      >>= \(ScriptDeployment txIn scriptStatus) →
        case scriptStatus of
          ScriptStatusDeployCompleted →
            guard (txIn' == txIn)
              $> ScriptDeploymentConfirmationDuplicate (Just scriptStatus) txIn'
          ScriptStatusDeployInitiated →
            guard (txIn' /= txIn)
              $> ScriptDeploymentConfirmationMismatch (Just scriptStatus) txIn'

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
    guard (addr == outputAddr) $> entry

-- | Finalize the UTxO set up to the given slot (inclusive).
finalise ∷ SlotNo → Utxo → Utxo
finalise
  immutableSlot
  utxo@Utxo {scriptDeployments, reversibleUpdates, finalEntries} =
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

{- | All entries in the UTxO set: both reversible and final inputs.
May include already used inputs.
-}
allEntries ∷ Utxo → Map TxIn (LedgerAddress, Value)
allEntries Utxo {reversibleUpdates, finalEntries} =
  updatesToEntries reversibleUpdates finalEntries

{- | Spendable entries in the UTxO set:
Everything that could be spent (i.e. not already used),
including both reversible and final inputs.
-}
spendableEntries ∷ Utxo → Map TxIn (LedgerAddress, Value)
spendableEntries utxo = Map.withoutKeys (allEntries utxo) (usedInputs utxo)

txInputs ∷ Utxo → Set TxIn
txInputs = Map.keysSet . allEntries

spendableTxInputs ∷ Utxo → Set TxIn
spendableTxInputs = Map.keysSet . spendableEntries

totalValue ∷ Utxo → Value
totalValue =
  Map.foldr' (\(_addr, value) acc → value <> acc) mempty . spendableEntries

-- | Converts a list of UTxO updates to a map of UTxO entries.
updatesToEntries
  ∷ [(SlotNo, NonEmpty Update)]
  → Map TxIn (LedgerAddress, Value)
  → Map TxIn (LedgerAddress, Value)
updatesToEntries allUpdates entries = foldr overUpdates entries allUpdates
 where
  overUpdates
    ∷ (SlotNo, NonEmpty Update)
    → Map TxIn (LedgerAddress, Value)
    → Map TxIn (LedgerAddress, Value)
  overUpdates (_slot, !updates) = foldr overUpdate id (toList updates)
   where
    overUpdate
      ∷ Update
      → (Map TxIn (LedgerAddress, Value) → Map TxIn (LedgerAddress, Value))
      → (Map TxIn (LedgerAddress, Value) → Map TxIn (LedgerAddress, Value))
    overUpdate !update !prevUpdate = case update of
      ConfirmScriptDeployment _hash _input → prevUpdate
      SpendTxInput input → Map.delete input . prevUpdate
      AddSpendableTxInput input addr val →
        Map.insert input (addr, val) . prevUpdate
