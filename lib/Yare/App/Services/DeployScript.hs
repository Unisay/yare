module Yare.App.Services.DeployScript
  ( service
  , scriptDeployments
  , ScriptStatus (..)
  , DeployScriptError (..)
  ) where

import Yare.Prelude hiding (show)

import Cardano.Api.Ledger (Credential, KeyRole (DRepRole))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , BabbageEraOnwards (..)
  , BuildTx
  , BuildTxWith (BuildTxWith)
  , ConwayEraOnwards (..)
  , CtxTx
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , PlutusScriptV3
  , PoolId
  , ReferenceScript (..)
  , Script (..)
  , ScriptHash
  , ShelleyBasedEra
  , StakeCredential
  , Tx (..)
  , TxBodyContent (..)
  , TxId
  , TxIn (..)
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxIx (..)
  , TxOut (..)
  , TxOutDatum (..)
  , Witness (KeyWitness)
  , addTxOut
  , constructBalancedTx
  , convert
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
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Exception (throwIO)
import Control.Monad.Except (Except, throwError)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Text.Pretty.Simple (pShow)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..))
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage
  ( Storage (..)
  , StorageMgr (..)
  , defaultStorage
  , readDefaultStorage
  )
import Yare.Submitter qualified as Submitter

import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction
  ( mkCardanoApiUtxo
  , mkScriptOutput
  , witnessUtxoEntry
  )
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
      shelleyBasedEra ∷ ShelleyBasedEra era = convert currentEra
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
  let
    NetworkInfo
      { protocolParameters
      , epochInfo
      , systemStart
      , currentEra
      , network
      } = networkInfo

    shelleyBasedEra ∷ ShelleyBasedEra era = convert currentEra
    babbageEraOnwards ∷ BabbageEraOnwards era = convert currentEra

  utxoEntryForFee ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputFee addresses)
      >>= maybe (throwError (DeployScriptError NoFeeInputs)) pure

  utxoEntryForCollateral ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputCollateral addresses)
      >>= maybe (throwError (DeployScriptError NoCollateralInputs)) pure

  let
    wrapError =
      DeployScriptError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

  -- Pure construction of the transaction:
  either (throwError . wrapError) pure do
    let
      overrideKeyWitnesses ∷ Maybe Word =
        Nothing

      registeredPools ∷ Set PoolId =
        Set.empty

      delegations ∷ Map StakeCredential L.Coin =
        Map.empty

      rewards ∷ Map (Credential DRepRole StandardCrypto) L.Coin =
        Map.empty

      changeAddress ∷ AddressInEra era =
        fromShelleyAddrIsSbe shelleyBasedEra . ledgerAddress $
          Address.useForChange addresses

      scriptOutput ∷ TxOut CtxTx era =
        mkScriptOutput
          shelleyBasedEra
          protocolParameters
          (Address.forScript network (toShelleyScriptHash scriptHash))
          TxOutDatumNone
          (ReferenceScript babbageEraOnwards (toScriptInAnyLang plutusScript))

      txInsCollateral ∷ TxInsCollateral era =
        case currentEra of
          ConwayEraOnwardsConway →
            TxInsCollateral
              AlonzoEraOnwardsConway
              [Utxo.utxoEntryInput utxoEntryForCollateral]

      bodyContent ∷ TxBodyContent BuildTx era =
        defaultTxBodyContent shelleyBasedEra
          & setTxIns
            [
              ( Utxo.utxoEntryInput utxoEntryForFee
              , BuildTxWith (KeyWitness KeyWitnessForSpending)
              )
            ]
          & setTxInsCollateral txInsCollateral
          & addTxOut scriptOutput -- The script output has index 0
    tx ←
      constructBalancedTx
        shelleyBasedEra
        bodyContent
        changeAddress
        overrideKeyWitnesses
        (mkCardanoApiUtxo currentEra [utxoEntryForFee])
        protocolParameters
        epochInfo
        systemStart
        registeredPools
        delegations
        rewards
        [ witnessUtxoEntry utxoEntryForFee
        , witnessUtxoEntry utxoEntryForCollateral
        ]

    let
      -- TxIn containing the script output
      txId = getTxId (getTxBody tx)
      txIx = TxIx 0 -- `constructBalancedTx` appends change outputs to the tail
      -- of the outputs list so our script output index remains 0
      txIn = TxIn txId txIx

    pure (tx, txIn)

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

newtype DeployScriptError = DeployScriptError TxConstructionError
  deriving newtype (Show)
  deriving anyclass (Exception)
