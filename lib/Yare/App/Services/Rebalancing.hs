module Yare.App.Services.Rebalancing
  ( Error (..)
  , service
  ) where

import Yare.Prelude hiding (toList)

import Cardano.Api (TxId, TxOut (TxOut), inAnyShelleyBasedEra, setTxIns)
import Cardano.Api.Ledger.Lens (mkAdaValue)
import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , BuildTx
  , BuildTxWith (..)
  , ConwayEraOnwards (..)
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
  , UTxO
  , Witness (KeyWitness)
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
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (Except)
import Data.List (zipWith3)
import Data.Map.Strict qualified as Map
import GHC.IsList (IsList (..))
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey (..), Addresses (externalAddresses))
import Yare.Address qualified as Address
import Yare.Address qualified as Addresses
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction (minAdaValue, mkCardanoApiUtxo, witnessUtxoEntry)
import Yare.Utxo (Entry (..), Utxo, spendableEntries)
import Yare.Utxo qualified as Utxo

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

  utxoEntryForFee ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputFee addresses (0 ∷ Lovelace))
      >>= maybe (throwError (RebalancingTxError NoFeeInputs)) pure

  utxoEntryForCollateral ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputCollateral addresses (0 ∷ Lovelace))
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
            (mkAdaValue shelleyBasedEra (Coin 0))
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
      expDistribute
        (totalLovelaceBalance - sum minUtxoValues)
        (length rebalancingAddresses)
        (1.5 ∷ Double)
  let
    txOuts =
      zipWith3
        constructTxOut
        (toList rebalancingAddresses)
        distributedLovelace
        (toList minUtxoValues)

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra
        & setTxIns txIns
        & setTxOuts txOuts
        & setTxInsCollateral txInsCollateral
        & setTxProtocolParams
          (BuildTxWith (Just (LedgerProtocolParameters protocolParams)))

    inputsForBalancing ∷ UTxO era =
      mkCardanoApiUtxo era ([utxoEntryForFee, utxoEntryForCollateral] <> rebalanceEntries)

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
        guard (length (toList value) == 1)
        pure case Addresses.asOwnAddress addresses outputAddr of
          Nothing → impossible "useInputsForRebalancing: UTxO entry address is not own"
          Just MkAddressWithKey {..} →
            MkEntry
              { utxoEntryInput = input
              , utxoEntryValue = value
              , utxoEntryKey = paymentKey
              , utxoEntryAddress = ledgerAddress
              }
  pure (utxo, entries)

--------------------------------------------------------------------------------
-- Exponencial distribution ----------------------------------------------------

expDistribute
  ∷ ∀ a n
   . RealFrac a
  ⇒ Integral n
  ⇒ Lovelace
  -- ^ Total balance > 0
  → n
  -- ^ Number of outputs > 0
  → a
  -- ^ Exponencial function base > 1
  → Either Text [Lovelace]
expDistribute total n a
  | n <= 0 = Left "Number of outputs must be > 0."
  | total <= 0 = Left "Total balance must be > 0."
  | a <= 1 = Left "Exponencial function base must be > 1."
  | otherwise = do
      let
        weights = fromList [a ^^ i | i ← [1 .. n]]
        distributedLovelace =
          weights
            <&> \w → floor (fromInteger (unCoin total) * (w / sum weights))
        finalDifference = total - sum distributedLovelace
      Right ((head distributedLovelace + finalDifference) : tail distributedLovelace)

data Error
  = RebalancingTxError TxConstructionError
  | CalculateTotalBalanceError
  | NotEnoughFundsToRebalance
  | DistributionError Text
  deriving anyclass (Exception)
  deriving stock (Show)
