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
import Control.Lens (Lens, Simple, lens, (.~), (^.), _1)
import Control.Monad.Except (Except)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Text.Pretty.Simple (pShow)
import Yare.Address (AddressWithKey (..), Addresses, externalAddresses)
import Yare.Address qualified as Address
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
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)
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

mkServices ∷ Storage IO AppState → Submitter.Q → NetworkInfo era → Services IO
mkServices storage submitQ networkInfo =
  Services
    { serveAddresses =
        toList . fmap ledgerAddress . externalAddresses
          <$> readsStorage storage addressState
    , serveChangeAddresses =
        pure . ledgerAddress . snd . Address.useForChange
          <$> readsStorage storage addressState
    , serveFeeAddresses =
        pure . ledgerAddress . snd . Address.useForFees
          <$> readsStorage storage addressState
    , serveCollateralAddresses =
        pure . ledgerAddress . snd . Address.useForCollateral
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
  ∷ ∀ era e
   . ( e `CouldBe` InAnyShelleyBasedEra TxBodyErrorAutoBalance
     , e `CouldBe` NoFeeInputs
     , e `CouldBe` NoCollateralInputs
     )
  ⇒ NetworkInfo era
  → Script PlutusScriptV3
  → StateT (Addresses, Utxo) (Except (Variant e)) (Tx era)
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
    statefulMaybe Funds.useFeeInputs
      & Oops.onNothingThrow NoFeeInputs

  colInputs ∷ NonEmpty (TxIn, (AddressWithKey, Value)) ←
    statefulMaybe Funds.useCollateralInputs
      & Oops.onNothingThrow NoCollateralInputs

  AddressWithKey {ledgerAddress = changeAddr} ←
    stateful' _1 Address.useForChange

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
