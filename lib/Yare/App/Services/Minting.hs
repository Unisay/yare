module Yare.App.Services.Minting where

import Yare.Prelude hiding (show)

import Cardano.Address.Style.Shelley qualified as CAddr
import Cardano.Api.Ledger (Credential, KeyRole (DRepRole))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , AssetName
  , BuildTx
  , BuildTxWith (BuildTxWith)
  , ConwayEraOnwards (..)
  , CtxTx
  , ExecutionUnits (ExecutionUnits)
  , Inject (..)
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , MaryEraOnwards (..)
  , PlutusScriptOrReferenceInput (PScript)
  , PlutusScriptV3
  , PolicyId
  , PoolId
  , ReferenceScript (..)
  , Script (..)
  , ScriptData (..)
  , ScriptDatum (NoScriptDatumForMint)
  , ScriptLanguageInEra (..)
  , ScriptWitness (PlutusScriptWitness)
  , ShelleyBasedEra
  , ShelleyWitnessSigningKey
  , StakeCredential
  , Tx (..)
  , TxBodyContent (..)
  , TxBodyErrorAutoBalance
  , TxId (..)
  , TxIn (..)
  , TxInMode (..)
  , TxIns
  , TxInsCollateral (..)
  , TxIx (..)
  , TxMintValue (..)
  , TxOut
  , TxOutDatum (..)
  , Value
  , WitCtxMint
  , WitCtxTxIn
  , Witness (KeyWitness)
  , addTxOut
  , constructBalancedTx
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , getTxBody
  , getTxId
  , inAnyShelleyBasedEra
  , runExcept
  , serialiseToRawBytes
  , setTxIns
  , setTxInsCollateral
  , setTxMintValue
  , unPolicyId
  , unsafeHashableScriptData
  )
import Cardano.Crypto.Wallet qualified as CC
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Monad.Except (Except, throwError)
import Data.IntCast (intCast)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Plutus.Prelude qualified as Plutus
import Text.Pretty.Simple (pShow)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..), addressWitnessSigningKey)
import Yare.App.Scripts.MintingPolicy qualified as MintingPolicy
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (stateMay)
import Yare.Util.Tx.Construction (mkScriptOutput, mkUtxoFromInputs)
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

-- | Mint a token
service
  ∷ ∀ era state env
   . ( [Utxo, Tagged "submitted" (Set TxId)] ∈∈ state
     , [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
     )
  ⇒ env
  → PolicyId
  → AssetName
  → IO (Either Error TxId)
service env policy asset = do
  let NetworkInfo {currentEra = currentEra ∷ ConwayEraOnwards era} = look env
  let submitQueue ∷ Submitter.Q = look env
  let storageManager ∷ StorageMgr IO state = look env
  setStorageMode storageManager Durable
  overDefaultStorage storageManager mint' $ traverse \tx -> do
    putTextLn "Submitting the transaction:"
    putTextLn . toStrict $ pShow tx
    let txInMode = TxInMode (inject currentEra) tx
    getTxId (getTxBody tx) <$ Submitter.submit submitQueue txInMode
 where
  mint' ∷ state → (state, Either Error (Tx era))
  mint' s =
    case runExcept (runStateT (mint env policy asset) s) of
      Left e → (s, Left e)
      Right (tx, s') → (s', Right tx)

mint
  ∷ ∀ state env era
   . ( [Tagged "submitted" (Set TxId), Utxo] ∈∈ state
     , [Addresses, NetworkInfo era] ∈∈ env
     )
  ⇒ env
  → PolicyId
  → AssetName
  → StateT state (Except Error) (Tx era)
mint env policy asset = do
  let
    addresses = look @Addresses env
    networkInfo = look @(NetworkInfo era) env

  singleShotInput@(singleShotTxIn, (_addrWithKey, _value)) ←
    stateMay (Utxo.useFeeInputs addresses)
      >>= maybe (throwError (MintingTxError NoFeeInputs)) (pure . head)
  let
    whoCanMint =
      paymentKey (Address.useForChange addresses)
        & CAddr.getKey
        & CC.toXPub
        & CC.unXPub
        & Plutus.toBuiltin
        & Plutus.PubKeyHash

    (script, scriptHash) =
      MintingPolicy.serialised (whoCanMint, txInToOutRef singleShotTxIn)

  unless (serialiseToRawBytes (unPolicyId policy) == scriptHash) do
    throwError (InvalidPolicyId policy)
  tx ← constructTx addresses networkInfo policy asset script singleShotInput
  modify' $ updateTagged @"submitted" (Set.insert (getTxId (getTxBody tx)))
  pure tx

constructTx
  ∷ ∀ state era
   . Utxo ∈ state
  ⇒ Addresses
  → NetworkInfo era
  → PolicyId
  -- ^ The policy id of the minting policy.
  → AssetName
  -- ^ The name of the asset being minted.
  → Script PlutusScriptV3
  -- ^ The minting policy script.
  → (TxIn, (AddressWithKey, Value))
  -- ^ The single-shot tx input.
  → StateT state (Except Error) (Tx era)
constructTx addresses networkInfo policy asset plutusScript singleShotTxIn = do
  let
    NetworkInfo
      { protocolParameters
      , epochInfo
      , systemStart
      , currentEra = conwayEraOnwards
      } = networkInfo

    shelleyBasedEra ∷ ShelleyBasedEra era =
      inject conwayEraOnwards

    maryEraOnwards ∷ MaryEraOnwards era =
      case conwayEraOnwards of ConwayEraOnwardsConway → MaryEraOnwardsConway

    alonzoEraOnwards ∷ AlonzoEraOnwards era =
      case conwayEraOnwards of ConwayEraOnwardsConway → AlonzoEraOnwardsConway

    changeAddress ∷ AddressInEra era =
      fromShelleyAddrIsSbe
        shelleyBasedEra
        (ledgerAddress (Address.useForChange addresses))

    wrapError ∷ TxBodyErrorAutoBalance era → Error =
      MintingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

  feeInputs ∷ [(TxIn, (AddressWithKey, Value))] ←
    stateMay (Utxo.useFeeInputs addresses) >>= \case
      Nothing → throwError (MintingTxError NoFeeInputs)
      Just xs → pure (toList xs)

  colInputs ∷ [(TxIn, (AddressWithKey, Value))] ←
    stateMay (Utxo.useCollateralInputs addresses) >>= \case
      Nothing → throwError (MintingTxError NoCollateralInputs)
      Just xs → pure (toList xs)

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

      txOut ∷ TxOut CtxTx era =
        mkScriptOutput
          shelleyBasedEra
          protocolParameters
          (ledgerAddress (Address.useForNFTs addresses))
          TxOutDatumNone
          ReferenceScriptNone

      txInsCollateral ∷ TxInsCollateral era =
        TxInsCollateral alonzoEraOnwards (fst <$> colInputs)

      txIns ∷ TxIns BuildTx era =
        [ forSpending txIn
        | (txIn, _) ← singleShotTxIn : toList feeInputs
        ]

      witnesses ∷ [ShelleyWitnessSigningKey] =
        [ addressWitnessSigningKey awk
        | (_, (awk, _)) ← singleShotTxIn : (feeInputs ++ colInputs)
        ]

      txMintValue ∷ TxMintValue BuildTx era =
        TxMintValue maryEraOnwards (Map.singleton policy [(asset, 1, wit)])
       where
        wit ∷ BuildTxWith BuildTx (ScriptWitness WitCtxMint era) =
          let
            PlutusScript vScript (PScript → pScript) = plutusScript
            lang = case conwayEraOnwards of
              ConwayEraOnwardsConway → PlutusScriptV3InConway
            redeemer = unsafeHashableScriptData (ScriptDataList [])
           in
            BuildTxWith $
              PlutusScriptWitness
                lang
                vScript
                pScript
                NoScriptDatumForMint
                redeemer
                (ExecutionUnits 0 0)

      bodyContent ∷ TxBodyContent BuildTx era =
        defaultTxBodyContent shelleyBasedEra
          & setTxInsCollateral txInsCollateral
          & setTxIns txIns
          & addTxOut txOut
          & setTxMintValue txMintValue

    constructBalancedTx
      shelleyBasedEra
      bodyContent
      changeAddress
      overrideKeyWitnesses
      (mkUtxoFromInputs conwayEraOnwards feeInputs)
      protocolParameters
      epochInfo
      systemStart
      registeredPools
      delegations
      rewards
      witnesses

--------------------------------------------------------------------------------
-- Utils -----------------------------------------------------------------------

txInToOutRef ∷ TxIn → Plutus.TxOutRef
txInToOutRef (TxIn singleShotTxId (TxIx singleShotIx)) =
  Plutus.TxOutRef
    (Plutus.TxId (Plutus.toBuiltin (serialiseToRawBytes singleShotTxId)))
    (intCast singleShotIx)

forSpending ∷ TxIn → (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
forSpending txIn = (txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

data Error
  = MintingTxError TxConstructionError
  | InvalidPolicyId PolicyId
  deriving stock (Show)
  deriving anyclass (Exception)
