module Yare.App.Services
  ( Services (..)
  , mkServices
  , NoFeeInputs
  , NoCollateralInputs
  ) where

import Relude

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
import Data.Row (Disjoint, (.!))
import Data.Set qualified as Set
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey (..), externalAddresses)
import Yare.Address qualified as Address
import Yare.App.Scripts (script)
import Yare.App.State qualified as Yare
import Yare.App.Types (NetworkInfo (..))
import Yare.Chain.Types (ChainTip, LedgerAddress)
import Yare.Funds (Funds, SomeFunds)
import Yare.Funds qualified as Funds
import Yare.Storage (Storage (..), readStorageField)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (stateField, stateMay)
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)
import Yare.Utxo qualified as Utxo

-- | Application services
data Services m = Services
  { serveAddresses ∷ m [LedgerAddress]
  , serveChangeAddresses ∷ m [LedgerAddress]
  , serveFeeAddresses ∷ m [LedgerAddress]
  , serveCollateralAddresses ∷ m [LedgerAddress]
  , serveUtxo ∷ m Utxo.Entries
  , serveTip ∷ m ChainTip
  , deployScript
      ∷ IO
          ( Either
              ( Variant
                  [ CardanoApplyTxErr StandardCrypto
                  , InAnyShelleyBasedEra TxBodyErrorAutoBalance
                  , NoFeeInputs
                  , NoCollateralInputs
                  ]
              )
              TxId
          )
  }

mkServices
  ∷ Storage IO Yare.State
  → Submitter.Q
  → NetworkInfo era
  → Services IO
mkServices storage submitQ networkInfo =
  Services
    { serveAddresses =
        toList . fmap ledgerAddress . externalAddresses
          <$> readStorageField storage #addresses
    , serveChangeAddresses =
        pure . ledgerAddress . snd . Address.useForChange
          <$> readStorageField storage #addresses
    , serveFeeAddresses =
        pure . ledgerAddress . snd . Address.useForFees
          <$> readStorageField storage #addresses
    , serveCollateralAddresses =
        pure . ledgerAddress . snd . Address.useForCollateral
          <$> readStorageField storage #addresses
    , serveUtxo =
        Utxo.spendableEntries . (.! #utxo) <$> readStorage storage
    , serveTip =
        (.! #chainTip) <$> readStorage storage
    , deployScript =
        serviceDeployScript storage submitQ networkInfo
    }

-- | Deploys a script on-chain by submitting a transaction.
serviceDeployScript
  ∷ ∀ r era e
   . ( Disjoint Funds r
     , e `CouldBe` CardanoApplyTxErr StandardCrypto
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ Storage IO (SomeFunds r)
  → Submitter.Q
  → NetworkInfo era
  → IO (Either (Variant e) TxId)
serviceDeployScript storage submitQ networkInfo = do
  let NetworkInfo {currentEra} = networkInfo
      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra
  overStorage storage (run (txDeployScript networkInfo script)) >>= \case
    Left err → pure (Left err)
    Right tx@(Tx body _witnesses) → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      errs ← Submitter.submit submitQ (TxInMode shelleyBasedEra tx)
      pure $ maybeToLeft (getTxId body) errs
 where
  run ∷ StateT s (Except (Variant e)) a → (s → (s, Either (Variant e) a))
  run st =
    runStateT st & \f s →
      f s & \et →
        case runExcept et of
          Left e → (s, Left e)
          Right (txEra, s') → (s', Right txEra)

txDeployScript
  ∷ ∀ r era e
   . ( Disjoint Funds r
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → Script PlutusScriptV3
  → StateT (SomeFunds r) (Except (Variant e)) (Tx era)
txDeployScript networkInfo plutusScript = do
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
