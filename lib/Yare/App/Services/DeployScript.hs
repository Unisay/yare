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
  , TxIn
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxOut (..)
  , TxOutDatum (..)
  , Value
  , Witness (KeyWitness)
  , addTxOut
  , babbageEraOnwardsToShelleyBasedEra
  , constructBalancedTx
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
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
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Text.Pretty.Simple (pShow)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..))
import Yare.App.Scripts (script)
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (LedgerAddress)
import Yare.Funds (HasFunds)
import Yare.Funds qualified as Funds
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter
import Yare.Util.State (overStateField, stateField, stateMay)
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)

-- | Deploys a script on-chain by submitting a transaction.
service
  ∷ ∀ r era e state
   . ( HasFunds r
     , r .! "submitted" ≈ [TxId]
     , state ≈ Rec r
     , e `CouldBe` CardanoApplyTxErr StandardCrypto
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ Storage IO state
  → Submitter.Q
  → NetworkInfo era
  → IO (Either (Variant e) TxId)
service storage submitQ networkInfo = do
  let NetworkInfo {currentEra} = networkInfo
      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra

  overStorage storage makeAndSubmitScriptTx \case
    Left err → pure (Left err)
    Right tx@(Tx body _witnesses) → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      maybeToLeft (getTxId body)
        <$> Submitter.submit submitQ (TxInMode shelleyBasedEra tx)
 where
  makeAndSubmitScriptTx ∷ state → (state, Either (Variant e) (Tx era))
  makeAndSubmitScriptTx s =
    case runExcept (runStateT (deployScript networkInfo script) s) of
      Left e → (s, Left e)
      Right (txEra, s') → (s', Right txEra)

deployScript
  ∷ ∀ r era e
   . ( HasFunds r
     , r .! "submitted" ≈ [TxId]
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → Script PlutusScriptV3
  → StateT (Rec r) (Except (Variant e)) (Tx era)
deployScript networkInfo plutusScript =
  constructTx networkInfo plutusScript
    >>= \tx@(Tx (getTxId → txId) _witnesses) →
      tx <$ overStateField #submitted (txId :)

constructTx
  ∷ ∀ r era e
   . ( HasFunds r
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → Script PlutusScriptV3
  → StateT (Rec r) (Except (Variant e)) (Tx era)
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
        & addTxOut scriptOutput

    witnesses ∷ [ShelleyWitnessSigningKey]
    witnesses =
      [ witness addr
      | (_txIn, (addr, _value)) ← toList (feeInputs <> colInputs)
      ]

  -- Pure construction of the transaction:
  Oops.hoistEither . first (inAnyShelleyBasedEra shelleyBasedEra) $
    let
      overrideKeyWitnesses ∷ Maybe Word = Nothing
      registeredPools ∷ Set PoolId = Set.empty
      delegations ∷ Map StakeCredential L.Coin = Map.empty
      rewards ∷ Map (Credential DRepRole StandardCrypto) L.Coin = Map.empty
     in
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

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

data NoFeeInputs = NoFeeInputs
  deriving stock (Show)

data NoCollateralInputs = NoCollateralInputs
  deriving stock (Show)
