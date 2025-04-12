module Yare.App.Services.Rebalancing
  ( Error (..)
  , service
  ) where

import Yare.Prelude

import Cardano.Api (TxId, TxOut (TxOut), inAnyShelleyBasedEra, setTxIns)
import Cardano.Api.Ledger.Lens (mkAdaValue)
import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , BuildTx
  , BuildTxWith (..)
  , ConwayEraOnwards (..)
  , CtxTx
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , LedgerProtocolParameters (..)
  , Lovelace
  , ReferenceScript (..)
  , ShelleyBasedEra
  , Tx (..)
  , TxBodyContent
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxOutDatum (TxOutDatumNone)
  , TxOutValue
  , UTxO
  , Witness (KeyWitness)
  , calculateMinimumUTxO
  , constructBalancedTx
  , convert
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , getTxBody
  , getTxId
  , lovelaceToTxOutValue
  , runExcept
  , selectLovelace
  , setTxInsCollateral
  , setTxOuts
  , setTxProtocolParams
  )
import Cardano.Ledger.Coin (Coin (..))
import Control.Exception (throwIO)
import Control.Lens.Combinators
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (Except)
import Data.List (zipWith3)
import Data.Map.Strict qualified as Map
import GHC.IsList qualified as GHC
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey (..), Addresses (externalAddresses))
import Yare.Address qualified as Address
import Yare.Address qualified as Addresses
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction
  ( minAdaValue
  , mkCardanoApiUtxo
  , witnessUtxoEntry
  )
import Yare.Utxo (Entry (..), Utxo, spendableEntries)
import Yare.Utxo qualified as Utxo
import Control.Lens ((%~))

service
  ∷ ∀ era state env
   . ( [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
     , '[Utxo] ∈∈ state
     )
  ⇒ env
  → IO TxId
service env = do
  let NetworkInfo {currentEra} = look @(NetworkInfo era) env
  let submitQueue ∷ Submitter.Q = look env
  let storageManager ∷ StorageMgr IO state = look env
  setStorageMode storageManager Durable
  overDefaultStorage storageManager rebalance' \case
    Left err → do
      putTextLn "Error while rebalancing:"
      putTextLn . toStrict $ pShow err
      throwIO err
    Right tx → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      let txInMode = TxInMode (convert currentEra) tx
      getTxId (getTxBody tx)
        <$ Submitter.submit submitQueue txInMode
 where
  rebalance' ∷ state → (state, Either Error (Tx era))
  rebalance' s =
    case runExcept (runStateT (rebalance env) s) of
      Left e → (s, Left e)
      Right (r, s') → (s', Right r)

rebalance
  ∷ ∀ state env era
   . ( '[NetworkInfo era, Addresses] ∈∈ env
     , '[Utxo] ∈∈ state
     )
  ⇒ env
  → StateT state (Except Error) (Tx era)
rebalance env = do
  let
    addresses = look @Addresses env
    rebalancingAddresses = externalAddresses addresses
    network ∷ NetworkInfo era = look env
    era ∷ ConwayEraOnwards era = currentEra network
    epoch = epochInfo network
    protocolParams = protocolParameters network
    shelleyBasedEra ∷ ShelleyBasedEra era = convert era

    changeAddress ∷ AddressInEra era =
      fromShelleyAddrIsSbe shelleyBasedEra . ledgerAddress $
        Address.useForChange addresses

    minUtxoAda ∷ Lovelace =
      calculateMinimumUTxO shelleyBasedEra dummyTxOut protocolParams
     where
      dummyTxOut ∷ TxOut CtxTx era =
        TxOut changeAddress dummyBalance TxOutDatumNone ReferenceScriptNone
      dummyBalance ∷ TxOutValue era =
        lovelaceToTxOutValue shelleyBasedEra 10_000_000

    expectedTotalFee ∷ Lovelace = 500_000
    expectedTotalCollateral ∷ Lovelace = 600_000

  utxoEntryForFee ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputFee addresses (minUtxoAda + expectedTotalFee))
      >>= maybe (throwError (RebalancingTxError NoFeeInputs)) pure

  utxoEntryForCollateral ∷ Utxo.Entry ←
    usingMonadState
      ( Utxo.useInputCollateral
          addresses
          (minUtxoAda + expectedTotalCollateral)
      )
      >>= maybe (throwError (RebalancingTxError NoCollateralInputs)) pure

  totalLovelaceBalance ∷ Lovelace ←
    usingMonadState (calculateTotalBalance rebalancingAddresses)
      >>= maybe (throwError CalculateTotalBalanceError) pure

  rebalanceEntries ∷ [Utxo.Entry] ←
    usingMonadState (useInputsForRebalancing addresses)
      >>= maybe (impossible "useInputsForRebalancing resulted in Nothing") pure

  let
    txIns =
      (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . Utxo.utxoEntryInput
        <$> utxoEntryForFee : rebalanceEntries

    txInsCollateral ∷ TxInsCollateral era =
      case era of
        ConwayEraOnwardsConway →
          TxInsCollateral
            AlonzoEraOnwardsConway
            [Utxo.utxoEntryInput utxoEntryForCollateral]

    minUtxoValues ∷ NonEmpty Lovelace =
      rebalancingAddresses
        <&> \address →
          minAdaValue
            shelleyBasedEra
            (LedgerProtocolParameters protocolParams)
            (ledgerAddress address)
            (mkAdaValue shelleyBasedEra 0)
            TxOutDatumNone
            ReferenceScriptNone

    constructTxOut addr lovelace minUtxoValue =
      TxOut
        (fromShelleyAddrIsSbe shelleyBasedEra (Address.ledgerAddress addr))
        (lovelaceToTxOutValue shelleyBasedEra (lovelace + minUtxoValue))
        TxOutDatumNone
        ReferenceScriptNone

  distributedLovelace ←
    either (throwError . DistributionError) pure $
      exponentialDistribution
        (totalLovelaceBalance - sum minUtxoValues)
        (length rebalancingAddresses)
        (1.5 ∷ Double)
  let
    txOuts =
      zipWith3
        constructTxOut
        (GHC.toList rebalancingAddresses)
        distributedLovelace
        (GHC.toList minUtxoValues)

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra
        & setTxIns txIns
        & setTxOuts txOuts
        & setTxInsCollateral txInsCollateral
        & setTxProtocolParams
          (BuildTxWith (Just (LedgerProtocolParameters protocolParams)))

    inputsForBalancing ∷ UTxO era =
      mkCardanoApiUtxo
        era
        ( [utxoEntryForFee, utxoEntryForCollateral]
            <> rebalanceEntries
        )

    wrapError =
      RebalancingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

    shelleyWitSigningKeys =
      (witnessUtxoEntry <$> rebalanceEntries)
        <> [ witnessUtxoEntry utxoEntryForFee
           , witnessUtxoEntry utxoEntryForCollateral
           ]

  either (throwError . wrapError) pure do
    constructBalancedTx
      shelleyBasedEra
      bodyContent
      changeAddress
      empty {- overrideKeyWitnesses -}
      inputsForBalancing
      (LedgerProtocolParameters protocolParams)
      epoch
      (systemStart network)
      mempty {- registered pools -}
      mempty {- delegations      -}
      mempty {- rewards          -}
      shelleyWitSigningKeys

calculateTotalBalance ∷ NonEmpty AddressWithKey → Utxo → Maybe (Utxo, Lovelace)
calculateTotalBalance addresses utxo = Just (utxo, totalBalance)
 where
  totalBalance = selectLovelace $ Map.foldr' f mempty (spendableEntries utxo)
  f (addr, value) acc
    | addr `elem` (ledgerAddress <$> addresses) = acc <> value
    | otherwise = acc

useInputsForRebalancing ∷ Addresses → Utxo → Maybe (Utxo, [Utxo.Entry])
useInputsForRebalancing addresses utxo = do
  let entries = do
        (input, (outputAddr, value)) ← Map.toList (spendableEntries utxo)
        guard (length (GHC.toList value) == 1)
        pure case Addresses.asOwnAddress addresses outputAddr of
          Nothing →
            impossible "useInputsForRebalancing: UTxO entry address is not own"
          Just MkAddressWithKey {..} →
            MkEntry
              { utxoEntryInput = input
              , utxoEntryValue = value
              , utxoEntryKey = paymentKey
              , utxoEntryAddress = ledgerAddress
              }
  pure (utxo, entries)

exponentialDistribution
  ∷ ∀ a n
   . RealFrac a
  ⇒ Integral n
  ⇒ Lovelace
  -- ^ Total balance > 0
  → n
  -- ^ Number of outputs > 0
  → a
  -- ^ Exponential function base > 1
  → Either Text [Lovelace]
exponentialDistribution total n a
  | n <= 0 = Left "Number of outputs must be > 0."
  | total <= 0 = Left "Total balance must be > 0."
  | a <= 1 = Left "Exponential function base must be > 1."
  | otherwise = Right (assignExcess finalDifference distributedLovelace)
 where
  weights = fromList [a ^^ i | i ← [1 .. n]]
  distributedLovelace =
    weights <&> \w → floor (fromInteger (unCoin total) * (w / sum weights))
  finalDifference = total - sum distributedLovelace
  assignExcess excess = _head %~ (+ excess)

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

data Error
  = RebalancingTxError TxConstructionError
  | CalculateTotalBalanceError
  | DistributionError Text
  deriving anyclass (Exception)
  deriving stock (Show)
