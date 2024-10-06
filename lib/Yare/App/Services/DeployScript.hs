module Yare.App.Services.DeployScript
  ( service
  , NoFeeInputs
  , NoCollateralInputs
  ) where

import Yare.Prelude

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
import Yare.Funds qualified as Funds
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter
import Yare.Util.State (overStateField, stateField, stateMay)
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)
import Yare.Utxo (ScriptDeployment (Deployed), Utxo, setScriptDeployment)

-- | Deploys a script on-chain by submitting a transaction.
service
  ∷ ∀ r era e state
   . ( HasType "utxo" Utxo r
     , HasType "addresses" Addresses r
     , HasType "submitted" (List TxId) r
     , state ≈ Rec r
     , e `CouldBe` CardanoApplyTxErr StandardCrypto
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ Storage IO state
  → Submitter.Q
  → NetworkInfo era
  → IO (Either (Variant e) TxIn)
service storage submitQ networkInfo = do
  let NetworkInfo {currentEra} = networkInfo
      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra

  overStorage storage makeAndSubmitScriptTx \case
    Left err → pure (Left err)
    Right (tx, txInput) → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      maybeToLeft txInput
        <$> Submitter.submit submitQ (TxInMode shelleyBasedEra tx)
 where
  makeAndSubmitScriptTx ∷ state → (state, Either (Variant e) (Tx era, TxIn))
  makeAndSubmitScriptTx s =
    case runExcept (runStateT (deployScript networkInfo script) s) of
      Left e → (s, Left e)
      Right (txAndScriptDeployment, s') → (s', Right txAndScriptDeployment)

deployScript
  ∷ ∀ r era e
   . ( HasType "utxo" Utxo r
     , HasType "addresses" Addresses r
     , HasType "submitted" (List TxId) r
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → Script PlutusScriptV3
  → StateT
      (Rec r)
      (Except (Variant e))
      ( Tx era -- The transaction that deploys the script.
      , TxIn -- The input that references the script.
      )
deployScript networkInfo plutusScript = do
  res@(tx, txIn) ← constructTx networkInfo plutusScript
  let txId = getTxId (getTxBody tx)
  overStateField #submitted (txId :!)
  overStateField #utxo (setScriptDeployment (Deployed txIn))
  pure res

constructTx
  ∷ ∀ r era e
   . ( HasType "utxo" Utxo r
     , HasType "addresses" Addresses r
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → Script PlutusScriptV3
  → StateT (Rec r) (Except (Variant e)) (Tx era, TxIn)
constructTx networkInfo plutusScript = do
  let NetworkInfo
        { protocolParameters
        , epochInfo
        , systemStart
        , currentEra
        , network
        } = networkInfo

      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra

  feeInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    stateMay Funds.useFeeInputs
      & Oops.onNothingThrow NoFeeInputs

  colInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    stateMay Funds.useCollateralInputs
      & Oops.onNothingThrow NoCollateralInputs

  AddressWithKey {ledgerAddress = changeAddr} ←
    stateField #addresses Address.useForChange

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
      [ witness addr
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
