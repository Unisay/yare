{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Yare.App.Services.Rebalancing.Internal where

import Yare.Prelude

import Cardano.Api (TxId, TxOut (TxOut), inAnyShelleyBasedEra, setTxIns)
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
import Control.Lens ((%~))
import Control.Lens.Combinators (_last)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Error.Hoist ((<!?>))
import Control.Monad.Except (Except)
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey (..), Addresses (externalAddresses))
import Yare.Address qualified as Address
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction
  ( mkCardanoApiUtxo
  , witnessUtxoEntry
  )
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

service
  ∷ ∀ era state env
   . ( [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
     , Utxo ∈ state
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
   . ( [NetworkInfo era, Addresses] ∈∈ env
     , Utxo ∈ state
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

    expectedTotalFee ∷ Lovelace = 300_000
    expectedTotalCollateral ∷ Lovelace = 450_000

  utxoEntryForCollateral ∷ Utxo.Entry ←
    usingMonadState
      ( Utxo.useInputCollateral
          addresses
          (minUtxoAda + expectedTotalCollateral)
      )
      <!?> RebalancingTxError NoCollateralInputs

  totalLovelaceBalance ∷ Lovelace ←
    let addrs = toList (ledgerAddress <$> rebalancingAddresses)
     in selectLovelace . Utxo.totalAddressesValue addrs <$> gets look

  rebalanceEntries ∷ [Utxo.Entry] ←
    usingMonadState (Just . Utxo.useSpendableInputs addresses)
      <!?> impossible "useInputsForRebalancing resulted in Nothing"

  let
    txIns =
      (,BuildTxWith (KeyWitness KeyWitnessForSpending)) . Utxo.utxoEntryInput
        <$> rebalanceEntries

    txInsCollateral ∷ TxInsCollateral era =
      case era of
        ConwayEraOnwardsConway →
          TxInsCollateral
            AlonzoEraOnwardsConway
            [Utxo.utxoEntryInput utxoEntryForCollateral]

    minUtxoValuesSum ∷ Lovelace =
      minUtxoAda * fromIntegral (length rebalanceEntries)

  distributedLovelace ∷ [Lovelace] ←
    either (throwError . DistributionError) pure $
      exponentialDistribution
        (totalLovelaceBalance - minUtxoValuesSum - expectedTotalFee)
        (length rebalancingAddresses)
        (1.5 ∷ Double)
  let
    txOuts =
      zipWith
        constructTxOut
        [addr | MkAddressWithKey addr _key ← toList rebalancingAddresses]
        distributedLovelace
     where
      constructTxOut addr lovelace =
        TxOut
          (fromShelleyAddrIsSbe shelleyBasedEra addr)
          (lovelaceToTxOutValue shelleyBasedEra (lovelace + minUtxoAda))
          TxOutDatumNone
          ReferenceScriptNone

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra
        & setTxIns txIns
        & setTxOuts txOuts
        & setTxInsCollateral txInsCollateral
        & setTxProtocolParams
          (BuildTxWith (Just (LedgerProtocolParameters protocolParams)))

    inputsForBalancing ∷ UTxO era =
      mkCardanoApiUtxo era (utxoEntryForCollateral : rebalanceEntries)

    wrapError =
      RebalancingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

    shelleyWitSigningKeys =
      witnessUtxoEntry utxoEntryForCollateral
        : map witnessUtxoEntry rebalanceEntries

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
  assignExcess excess = _last %~ (+ excess)

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

data Error
  = RebalancingTxError TxConstructionError
  | CalculateTotalBalanceError
  | DistributionError Text
  deriving anyclass (Exception)
  deriving stock (Show)
