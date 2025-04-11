module Yare.App.Services.Rebalancing
  ( Error (..)
  , service
  ) where

import Yare.Prelude

import Cardano.Api (TxId, inAnyShelleyBasedEra, setTxIns)
import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , BuildTx
  , BuildTxWith (..)
  , ConwayEraOnwards (..)
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , LedgerProtocolParameters (..)
  , Lovelace
  , ShelleyBasedEra
  , Tx (..)
  , TxBodyContent
  , TxInMode (..)
  , TxInsCollateral (..)
  , UTxO
  , Witness (KeyWitness)
  , constructBalancedTx
  , convert
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , getTxBody
  , getTxId
  , runExcept
  , selectLovelace
  , setTxInsCollateral
  , setTxOuts
  , setTxProtocolParams
  )
import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (Except)
import Data.Map.Strict qualified as Map
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey (..), Addresses (externalAddresses))
import Yare.Address qualified as Address
import Yare.Address qualified as Addresses
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction (mkCardanoApiUtxo, witnessUtxoEntry)
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
    usingMonadState (calculateTotalBalance (externalAddresses addresses))
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

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra
        & setTxIns txIns
        & setTxOuts _
        & setTxInsCollateral txInsCollateral
        & setTxProtocolParams
          (BuildTxWith (Just (LedgerProtocolParameters protocolParams)))

    inputsForBalancing ∷ UTxO era =
      mkCardanoApiUtxo era ([utxoEntryForFee, utxoEntryForCollateral] <> rebalanceEntries)

    wrapError =
      RebalancingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

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
      [ witnessUtxoEntry utxoEntryForFee
      , witnessUtxoEntry utxoEntryForCollateral
      ]

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

data Error
  = RebalancingTxError TxConstructionError
  | CalculateTotalBalanceError
  deriving anyclass (Exception)
  deriving stock (Show)
