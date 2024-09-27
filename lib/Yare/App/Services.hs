{-# LANGUAGE TemplateHaskell #-}

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
  , LedgerProtocolParameters (unLedgerProtocolParameters)
  , PlutusScript (..)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , PoolId
  , ReferenceScript (..)
  , Script (..)
  , ShelleyWitnessSigningKey
  , StakeCredential
  , Tx
  , TxBodyContent (..)
  , TxBodyErrorAutoBalance
  , TxIn
  , TxInMode (..)
  , TxIns
  , TxInsCollateral (..)
  , TxOut (..)
  , TxOutDatum (..)
  , TxOutValue (..)
  , UTxO (..)
  , Value
  , Witness (KeyWitness)
  , addTxOut
  , babbageEraOnwardsToMaryEraOnwards
  , babbageEraOnwardsToShelleyBasedEra
  , calculateMinimumUTxO
  , constructBalancedTx
  , defaultTxBodyContent
  , fromShelleyAddr
  , fromShelleyAddrIsSbe
  , inAnyShelleyBasedEra
  , lovelaceToTxOutValue
  , runExcept
  , setTxIns
  , setTxInsCollateral
  , shelleyBasedEraConstraints
  , toLedgerValue
  , toScriptInAnyLang
  , unLedgerProtocolParameters
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Lens (Lens, Simple, lens, (.~), (^.), _1)
import Control.Monad.Except (Except)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Development.Placeholders (todo)
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Yare.Address (AddressWithKey (ledgerAddress))
import Yare.Addresses (Addresses, externalAddresses)
import Yare.Addresses qualified as Addresses
import Yare.App.Scripts (script)
import Yare.App.Types (AppState, NetworkInfo (..), addressState, chainState)
import Yare.Chain.Follower (ChainState (..), chainTip, utxo)
import Yare.Chain.Types (ChainTip, LedgerAddress)
import Yare.Funds qualified as Funds
import Yare.Storage
  ( Storage (..)
  , readsStorage
  , stateful'
  , statefulMaybe
  , zoomStorage
  )
import Yare.Submitter qualified as Submitter
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

-- | Application services
type Services ∷ (Type → Type) → Type
data Services m = Services
  { serveAddresses ∷ m [LedgerAddress]
  , serveChangeAddresses ∷ m [LedgerAddress]
  , serveFeeAddresses ∷ m [LedgerAddress]
  , serveCollateralAddresses ∷ m [LedgerAddress]
  , serveUtxo ∷ m Utxo.Entries
  , serveTip ∷ m ChainTip
  , deployScript
      ∷ IO
          ( Maybe
              ( Variant
                  [ CardanoApplyTxErr StandardCrypto
                  , InAnyShelleyBasedEra TxBodyErrorAutoBalance
                  , NoFeeInputs
                  , NoCollateralInputs
                  ]
              )
          )
  }

mkServices
  ∷ Storage IO AppState
  → Submitter.Q
  → NetworkInfo era
  → Services IO
mkServices storage submitQ networkInfo =
  Services
    { serveAddresses =
        toList . fmap ledgerAddress . externalAddresses
          <$> readsStorage storage addressState
    , serveChangeAddresses =
        pure . snd . Addresses.useForChange
          <$> readsStorage storage addressState
    , serveFeeAddresses =
        pure . snd . Addresses.useForFees
          <$> readsStorage storage addressState
    , serveCollateralAddresses =
        pure . snd . Addresses.useForCollateral
          <$> readsStorage storage addressState
    , serveUtxo =
        Utxo.spendableEntries
          <$> readsStorage chainStateStorage utxo
    , serveTip =
        readsStorage chainStateStorage chainTip
    , deployScript =
        serviceDeployScript
          (zoomStorage (both addressState (chainState . utxo)) storage)
          submitQ
          networkInfo
    }
 where
  chainStateStorage ∷ Storage IO ChainState
  chainStateStorage = zoomStorage chainState storage

-- | Deploys a script on-chain by submitting a transaction.
serviceDeployScript
  ∷ ∀ era e
   . ( e `CouldBe` CardanoApplyTxErr StandardCrypto
     , e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ Storage IO (Addresses, Utxo)
  → Submitter.Q
  → NetworkInfo era
  → IO (Maybe (Variant e))
serviceDeployScript storage submitQ networkInfo = do
  let NetworkInfo {currentEra} = networkInfo
      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra
  overStorage storage (run (txDeployScript networkInfo script)) >>= \case
    Left err → pure (Just err)
    Right signedBalancedTx →
      Submitter.submit submitQ (TxInMode shelleyBasedEra signedBalancedTx)
 where
  run ∷ StateT s (Except (Variant e)) a → (s → (s, Either (Variant e) a))
  run st =
    runStateT st & \f s →
      f s & \et →
        case runExcept et of
          Left e → (s, Left e)
          Right (txEra, s') → (s', Right txEra)

txDeployScript
  ∷ ∀ era e
   . ( e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → PlutusScript PlutusScriptV3
  → StateT (Addresses, Utxo) (Except (Variant e)) (Tx era)
txDeployScript networkInfo plutusScript = do
  let NetworkInfo
        { protocolParameters
        , epochInfo
        , systemStart
        , currentEra
        } = networkInfo

      shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra

  feeInputs ∷ NonEmpty (TxIn, (LedgerAddress, Value)) ←
    statefulMaybe Funds.useFeeInputs
      & Oops.onNothingThrow NoFeeInputs

  colInputs ∷ NonEmpty (TxIn, (LedgerAddress, Value)) ←
    statefulMaybe Funds.useCollateralInputs
      & Oops.onNothingThrow NoCollateralInputs

  changeAddr ←
    fromShelleyAddrIsSbe shelleyBasedEra
      <$> stateful' _1 Addresses.useForChange

  scriptAddr ←
    fromShelleyAddrIsSbe shelleyBasedEra
      <$> stateful' _1 Addresses.useForScript

  let
    scriptOutput ∷ TxOut CtxTx era
    scriptOutput =
      TxOut
        scriptAddr
        (lovelaceToTxOutValue shelleyBasedEra 0)
        TxOutDatumNone
        ( ReferenceScript
            currentEra
            (toScriptInAnyLang (PlutusScript PlutusScriptV3 plutusScript))
        )

    scriptOutputAdaValue =
      lovelaceToTxOutValue shelleyBasedEra $
        calculateMinimumUTxO
          shelleyBasedEra
          scriptOutput
          (unLedgerProtocolParameters protocolParameters)

    scriptOutputWithAda =
      let TxOut addr _value datum script' = scriptOutput
       in TxOut addr scriptOutputAdaValue datum script'

    txInsCollateral =
      let
        collInputs ∷ AlonzoEraOnwards era → TxInsCollateral era
        collInputs = (`TxInsCollateral` toList (fmap fst colInputs))
       in
        case currentEra of
          BabbageEraOnwardsBabbage → collInputs AlonzoEraOnwardsBabbage
          BabbageEraOnwardsConway → collInputs AlonzoEraOnwardsConway

    overrideKeyWitnesses ∷ Maybe Word
    overrideKeyWitnesses = Nothing

    txInputs ∷ UTxO era
    txInputs = UTxO $ Map.fromList do
      (txIn, (addr, value)) ← toList feeInputs
      pure
        ( txIn
        , TxOut
            (fromShelleyAddr shelleyBasedEra addr)
            ( shelleyBasedEraConstraints shelleyBasedEra $
                TxOutValueShelleyBased
                  shelleyBasedEra
                  ( toLedgerValue
                      (babbageEraOnwardsToMaryEraOnwards currentEra)
                      value
                  )
            )
            TxOutDatumNone
            ReferenceScriptNone
        )

    txIns ∷ TxIns BuildTx era
    txIns =
      (BuildTxWith (KeyWitness KeyWitnessForSpending) <$) <$> toList feeInputs

    bodyContent ∷ TxBodyContent BuildTx era =
      defaultTxBodyContent shelleyBasedEra
        & setTxIns txIns
        & setTxInsCollateral txInsCollateral
        & addTxOut scriptOutputWithAda

    registeredPools ∷ Set PoolId
    registeredPools = Set.empty

    delegations ∷ Map StakeCredential L.Coin
    delegations = Map.empty

    delegationsRewards ∷ Map (Credential DRepRole StandardCrypto) L.Coin
    delegationsRewards = Map.empty

    witnesses ∷ [ShelleyWitnessSigningKey]
    witnesses = [$(todo "witnesses")]

  Oops.hoistEither . first (inAnyShelleyBasedEra shelleyBasedEra) $
    constructBalancedTx
      shelleyBasedEra
      bodyContent
      changeAddr
      overrideKeyWitnesses
      txInputs
      protocolParameters
      epochInfo
      systemStart
      registeredPools
      delegations
      delegationsRewards
      witnesses

--------------------------------------------------------------------------------
-- Lens utilities --------------------------------------------------------------

both ∷ Simple Lens s a → Simple Lens s b → Simple Lens s (a, b)
both l r =
  lens
    (\s → (s ^. l, s ^. r))
    (\s (a, b) → s & l .~ a & r .~ b)

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

type NoFeeInputs ∷ Type
data NoFeeInputs = NoFeeInputs
  deriving stock (Show)

type NoCollateralInputs ∷ Type
data NoCollateralInputs = NoCollateralInputs
  deriving stock (Show)
