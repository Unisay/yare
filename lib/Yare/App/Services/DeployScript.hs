module Yare.App.Services.DeployScript
  ( service
  , scriptDeployments
  , ScriptStatus (..)
  , DeployScriptError (..)
  ) where

import Yare.Prelude hiding (show)

import Cardano.Address.Style.Shelley qualified as CAddr
import Cardano.Api.Ledger (Credential, KeyRole (DRepRole))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( AlonzoEraOnwards (..)
  , BabbageEraOnwards (..)
  , BuildTx
  , BuildTxWith (BuildTxWith)
  , CtxTx
  , InAnyShelleyBasedEra (..)
  , Inject (..)
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , PlutusScriptV3
  , PoolId
  , ReferenceScript (..)
  , Script (..)
  , ScriptHash
  , ShelleyWitnessSigningKey
  , StakeCredential
  , Tx (..)
  , TxBodyContent (..)
  , TxBodyErrorAutoBalance
  , TxId
  , TxIn (..)
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxIx (..)
  , TxOut (..)
  , TxOutDatum (..)
  , Value
  , Witness (KeyWitness)
  , addTxOut
  , constructBalancedTx
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , getTxBody
  , getTxId
  , inAnyShelleyBasedEra
  , runExcept
  , setTxIns
  , setTxInsCollateral
  , toScriptInAnyLang
  , toShelleyScriptHash
  )
import Cardano.Api.Shelley qualified as CApi
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Exception (throwIO)
import Control.Monad.Except (Except, throwError)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Text.Pretty.Simple (pShow)
import Text.Show (show)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Chain.Types (LedgerAddress)
import Yare.Storage (Storage (..), StorageMgr (..), defaultStorage, readDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (stateMay)
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)
import Yare.Utxo (ScriptDeployment, ScriptStatus, Utxo)
import Yare.Utxo qualified as Utxo

scriptDeployments
  ∷ ∀ state env
   . (StorageMgr IO state ∈ env, Utxo ∈ state)
  ⇒ env
  → IO (Map ScriptHash ScriptDeployment)
scriptDeployments env =
  Utxo.scriptDeployments . look @Utxo <$> readDefaultStorage @state env

-- | Deploys a script on-chain by submitting a transaction.
service
  ∷ ∀ era state env
   . ( [Utxo, Tagged "submitted" (Set TxId)] ∈∈ state
     , [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
     )
  ⇒ env
  → ScriptHash
  → Script PlutusScriptV3
  → IO TxIn
service env scriptHash script = do
  let NetworkInfo {currentEra} = look env
      shelleyBasedEra = inject @(BabbageEraOnwards era) currentEra
  let storageManager ∷ StorageMgr IO state = look env
  setStorageMode storageManager Durable
  storage ← defaultStorage storageManager
  overStorage storage makeAndSubmitScriptTx \case
    Left err → throwIO err
    Right (tx, txInput) → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      let submiteQueue ∷ Submitter.Q = look env
      txInput <$ Submitter.submit submiteQueue (TxInMode shelleyBasedEra tx)
 where
  makeAndSubmitScriptTx
    ∷ state → (state, Either DeployScriptError (Tx era, TxIn))
  makeAndSubmitScriptTx s =
    deployScript env scriptHash script
      & flip runStateT s
      & runExcept
      & \case
        Left e → (s, Left e)
        Right (txAndScriptDeployment, s') → (s', Right txAndScriptDeployment)

deployScript
  ∷ ∀ state env era
   . ( [Tagged "submitted" (Set TxId), Utxo] ∈∈ state
     , [Addresses, NetworkInfo era] ∈∈ env
     )
  ⇒ env
  → ScriptHash
  → Script PlutusScriptV3
  → StateT
      state
      (Except DeployScriptError)
      ( Tx era -- The transaction that deploys the script.
      , TxIn -- The input that references the script.
      )
deployScript env scriptHash plutusScript = do
  res@(tx, txIn) ←
    constructTx
      (look @Addresses env)
      (look @(NetworkInfo era) env)
      scriptHash
      plutusScript
  modify' $
    updateTagged @"submitted" (Set.insert (getTxId (getTxBody tx)))
      . update @Utxo (Utxo.initiateScriptDeployment scriptHash txIn)
  pure res

constructTx
  ∷ ∀ state era
   . Utxo ∈ state
  ⇒ Addresses
  → NetworkInfo era
  → ScriptHash
  → Script PlutusScriptV3
  → StateT state (Except DeployScriptError) (Tx era, TxIn)
constructTx addresses networkInfo scriptHash plutusScript = do
  let NetworkInfo
        { protocolParameters
        , epochInfo
        , systemStart
        , currentEra
        , network
        } = networkInfo

      shelleyBasedEra = inject currentEra

  feeInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    stateMay (Utxo.useFeeInputs addresses)
      >>= maybe (throwError NoFeeInputs) pure

  colInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    stateMay (Utxo.useCollateralInputs addresses)
      >>= maybe (throwError NoCollateralInputs) pure

  let AddressWithKey {ledgerAddress = changeAddr} =
        Address.useForChange addresses

  let
    scriptAddr ∷ LedgerAddress
    scriptAddr = Address.forScript network (toShelleyScriptHash scriptHash)

    scriptOutput ∷ TxOut CtxTx era =
      mkScriptOutput
        shelleyBasedEra
        protocolParameters
        scriptAddr
        TxOutDatumNone
        (ReferenceScript currentEra (toScriptInAnyLang plutusScript))

    txInsCollateral =
      let
        collInputs ∷ AlonzoEraOnwards era → TxInsCollateral era
        collInputs = (`TxInsCollateral` toList (fmap fst colInputs))
       in
        case currentEra of
          BabbageEraOnwardsBabbage → collInputs AlonzoEraOnwardsBabbage
          BabbageEraOnwardsConway → collInputs AlonzoEraOnwardsConway

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra
        & setTxIns
          [ (txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))
          | (txIn, _addrWithKeyAndValue) ← toList feeInputs
          ]
        & setTxInsCollateral txInsCollateral
        & addTxOut scriptOutput -- The script output has index 0
    witnesses ∷ [ShelleyWitnessSigningKey]
    witnesses =
      [ CApi.WitnessPaymentExtendedKey
        (CApi.PaymentExtendedSigningKey (CAddr.getKey (paymentKey addr)))
      | (_txIn, (addr, _value)) ← toList (feeInputs <> colInputs)
      ]

  let wrapError = TxAutoBalanceError . inAnyShelleyBasedEra shelleyBasedEra

  -- Pure construction of the transaction:
  either (throwError . wrapError) pure do
    let
      overrideKeyWitnesses ∷ Maybe Word = Nothing
      registeredPools ∷ Set PoolId = Set.empty
      delegations ∷ Map StakeCredential L.Coin = Map.empty
      rewards ∷ Map (Credential DRepRole StandardCrypto) L.Coin = Map.empty
    tx ←
      constructBalancedTx
        shelleyBasedEra
        bodyContent
        (fromShelleyAddrIsSbe shelleyBasedEra changeAddr)
        overrideKeyWitnesses
        (mkUtxoFromInputs currentEra feeInputs)
        protocolParameters
        epochInfo
        systemStart
        registeredPools
        delegations
        rewards
        witnesses

    let
      -- TxIn containing the script output
      txId = getTxId (getTxBody tx)
      txIx = TxIx 0 -- `constructBalancedTx` appends change outputs to the tail
      -- of the outputs list so our script output index remains 0
      txIn = TxIn txId txIx

    pure (tx, txIn)

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

data DeployScriptError
  = NoFeeInputs
  | NoCollateralInputs
  | TxAutoBalanceError (InAnyShelleyBasedEra TxBodyErrorAutoBalance)

instance Show DeployScriptError where
  show = \case
    NoFeeInputs →
      "No fee inputs available."
    NoCollateralInputs →
      "No collateral inputs available."
    TxAutoBalanceError (InAnyShelleyBasedEra era e) →
      "Tx balancing error in era " <> show era <> ": " <> show e

deriving anyclass instance Exception DeployScriptError
