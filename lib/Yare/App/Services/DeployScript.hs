module Yare.App.Services.DeployScript
  ( service
  , NoFeeInputs
  , NoCollateralInputs
  ) where

import Yare.Prelude

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
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , PlutusScriptV3
  , PoolId
  , ReferenceScript (..)
  , Script (..)
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
  , babbageEraOnwardsToShelleyBasedEra
  , constructBalancedTx
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , getTxBody
  , getTxId
  , hashScript
  , inAnyShelleyBasedEra
  , runExcept
  , setTxIns
  , setTxInsCollateral
  , toScriptInAnyLang
  , toShelleyScriptHash
  )
import Cardano.Api.Shelley qualified as CApi
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Hashes qualified as Ledger
import Control.Monad.Except (Except)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Strict (List ((:!)))
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Text.Pretty.Simple (pShow)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..))
import Yare.App.Scripts (script)
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (LedgerAddress)
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter
import Yare.Util.State (stateMay)
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)
import Yare.Utxo (ScriptDeployment (Deployed), Utxo, setScriptDeployment)
import Yare.Utxo qualified as Utxo

-- | Deploys a script on-chain by submitting a transaction.
service
  ∷ ∀ (era ∷ Type) state env e
   . ( [Utxo, List TxId] ∈∈ state
     , [Addresses, Submitter.Q, NetworkInfo era, Storage IO state] ∈∈ env
     , e `CouldBe` CardanoApplyTxErr StandardCrypto
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ env
  → IO (Either (Variant e) TxIn)
service env = do
  let NetworkInfo {currentEra} = look env
      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra

  overStorage (look @(Storage IO state) env) makeAndSubmitScriptTx \case
    Left err → pure (Left err)
    Right (tx, txInput) → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      maybeToLeft txInput
        <$> Submitter.submit
          (look @Submitter.Q env)
          (TxInMode shelleyBasedEra tx)
 where
  makeAndSubmitScriptTx ∷ state → (state, Either (Variant e) (Tx era, TxIn))
  makeAndSubmitScriptTx s =
    case runExcept (runStateT (deployScript env script) s) of
      Left e → (s, Left e)
      Right (txAndScriptDeployment, s') → (s', Right txAndScriptDeployment)

deployScript
  ∷ ∀ state env era e
   . ( [List TxId, Utxo] ∈∈ state
     , [Addresses, NetworkInfo era] ∈∈ env
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ env
  → Script PlutusScriptV3
  → StateT
      state
      (Except (Variant e))
      ( Tx era -- The transaction that deploys the script.
      , TxIn -- The input that references the script.
      )
deployScript env plutusScript = do
  res@(tx, txIn) ←
    constructTx
      (look @Addresses env)
      (look @(NetworkInfo era) env)
      plutusScript
  modify' \s →
    s
      & setter (getTxId (getTxBody tx) :! look @(List TxId) s)
      & setter (setScriptDeployment (Deployed txIn) (look @Utxo s))
  pure res

constructTx
  ∷ ∀ state era e
   . ( Utxo ∈ state
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ Addresses
  → NetworkInfo era
  → Script PlutusScriptV3
  → StateT state (Except (Variant e)) (Tx era, TxIn)
constructTx addresses networkInfo plutusScript = do
  let NetworkInfo
        { protocolParameters
        , epochInfo
        , systemStart
        , currentEra
        , network
        } = networkInfo

      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra

  feeInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    stateMay (Utxo.useFeeInputs addresses)
      & Oops.onNothingThrow NoFeeInputs

  colInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    stateMay (Utxo.useCollateralInputs addresses)
      & Oops.onNothingThrow NoCollateralInputs

  let AddressWithKey {ledgerAddress = changeAddr} =
        Address.useForChange addresses

  let
    scriptHash ∷ Ledger.ScriptHash StandardCrypto
    scriptHash = toShelleyScriptHash (hashScript plutusScript)

    scriptAddr ∷ LedgerAddress
    scriptAddr = Address.forScript network scriptHash

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

  -- Pure construction of the transaction:
  Oops.hoistEither $ first (inAnyShelleyBasedEra shelleyBasedEra) do
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

data NoFeeInputs = NoFeeInputs
  deriving stock (Show)

data NoCollateralInputs = NoCollateralInputs
  deriving stock (Show)
