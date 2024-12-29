module Yare.App.Services.Minting where

import Yare.Prelude hiding (show)

import Cardano.Api.Ledger (Credential, KeyRole (DRepRole))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( AddressInEra
  , AssetName
  , BuildTx
  , BuildTxWith (BuildTxWith)
  , ConwayEraOnwards (..)
  , CtxTx
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , PlutusScriptOrReferenceInput (PScript)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , PolicyId
  , PoolId
  , Quantity
  , ReferenceScript (..)
  , ScriptLanguageInEra (..)
  , ShelleyBasedEra
  , StakeCredential
  , Tx (..)
  , TxBodyContent (..)
  , TxId
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxMintValue (..)
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
  , setTxMintValue
  )
import Cardano.Api.Shelley qualified as CApi
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Exception (throwIO)
import Control.Monad.Except (Except, throwError)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Plutus.Prelude qualified as Plutus
import Text.Pretty.Simple (pShow)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..))
import Yare.App.Scripts.MintingPolicy qualified as MintingPolicy
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Chain.Types (LedgerAddress)
import Yare.Compat.Plutus (pubKeyHashFromLedgerAddress, txOutRefFromTxIn)
import Yare.Storage (StorageMgr (..), overDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Util.State (usingMonadState)
import Yare.Util.Tx.Construction
  ( mkCardanoApiUtxo
  , mkScriptOutput
  , witnessUtxoEntry
  )
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

-- | Mint a token
service
  ∷ ∀ era state env
   . ( [Utxo, Tagged "submitted" (Set TxId)] ∈∈ state
     , [Addresses, Submitter.Q, NetworkInfo era, StorageMgr IO state] ∈∈ env
     )
  ⇒ env
  → AssetName
  → IO (PolicyId, TxId)
service env asset = do
  let NetworkInfo {currentEra} = look @(NetworkInfo era) env
  let submitQueue ∷ Submitter.Q = look env
  let storageManager ∷ StorageMgr IO state = look env
  setStorageMode storageManager Durable
  overDefaultStorage storageManager mint' \case
    Left err → throwIO err
    Right (policyId, tx) → do
      putTextLn "Submitting the transaction:"
      putTextLn . toStrict $ pShow tx
      let txInMode = TxInMode (convert currentEra) tx
      (policyId, getTxId (getTxBody tx))
        <$ Submitter.submit submitQueue txInMode
 where
  mint' ∷ state → (state, Either Error (PolicyId, Tx era))
  mint' s =
    case runExcept (runStateT (mint env asset) s) of
      Left e → (s, Left e)
      Right (r, s') → (s', Right r)

mint
  ∷ ∀ state env era
   . ( [Tagged "submitted" (Set TxId), Utxo] ∈∈ state
     , [Addresses, NetworkInfo era] ∈∈ env
     )
  ⇒ env
  → AssetName
  → StateT state (Except Error) (PolicyId, Tx era)
mint env asset = do
  let
    NetworkInfo {protocolParameters, epochInfo, systemStart, currentEra} =
      look @(NetworkInfo era) env

    addresses = look @Addresses env

    shelleyBasedEra ∷ ShelleyBasedEra era = convert currentEra

  utxoEntryForFee ∷ Utxo.Entry ←
    usingMonadState (Utxo.useInputFee addresses)
      >>= maybe (throwError (MintingTxError NoFeeInputs)) pure

  utxoEntryForCollateral@Utxo.MkEntry {utxoEntryAddress = collateralAddress} ←
    usingMonadState (Utxo.useInputCollateral addresses)
      >>= maybe (throwError (MintingTxError NoCollateralInputs)) pure

  Utxo.MkEntry {utxoEntryInput = singletonInput} ←
    usingMonadState (Utxo.useInputLowestAdaOnly addresses)
      >>= maybe (throwError (MintingTxError NoSingletonInputs)) pure

  whoCanMint ∷ Plutus.PubKeyHash ←
    -- Minting policy only allows to mint if Tx is signed by the owner of the
    -- public key given to it as a configuration parameter. In order not to
    -- add extra signature to the minting Tx (thus increasing its size) we use
    -- public key hash of the collateral address as its signature is added to
    -- the minting Tx anyway.
    case pubKeyHashFromLedgerAddress collateralAddress of
      Nothing → throwError (MintingTxError ScriptAddressNoPublicKeyHash)
      Just pkh → pure pkh

  let
    wrapError =
      MintingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

    (plutusScript, CApi.PolicyId → policy) =
      MintingPolicy.serialised
        MintingPolicy.MkMintingParams
          { whoCanMint
          , singletonTxOut =
              -- Minting policy is a singleton policy that allows only one
              -- minting ever. This is done by giving the minting policy a
              -- tx output reference as a configuration parameter, for the
              -- minting policy to check if the minting transaction also
              -- spends configured tx output. Because a tx output can only
              -- be spent once, this ensures that the minting policy can only
              -- be used once.
              txOutRefFromTxIn singletonInput
          }

  -- Pure construction of the transaction:
  tx ← either (throwError . wrapError) pure do
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

      outputAddress ∷ LedgerAddress =
        ledgerAddress (Address.useForMinting addresses)

      scriptOutput ∷ TxOut CtxTx era =
        mkScriptOutput
          shelleyBasedEra
          protocolParameters
          outputAddress
          TxOutDatumNone
          ReferenceScriptNone

      txInsCollateral ∷ TxInsCollateral era =
        case currentEra of
          ConwayEraOnwardsConway →
            TxInsCollateral
              CApi.AlonzoEraOnwardsConway
              [Utxo.utxoEntryInput utxoEntryForCollateral]

      txMintValue ∷ TxMintValue BuildTx era =
        let
          lang ∷ ScriptLanguageInEra PlutusScriptV3 era =
            case currentEra of ConwayEraOnwardsConway → PlutusScriptV3InConway

          wit ∷ BuildTxWith BuildTx (CApi.ScriptWitness CApi.WitCtxMint era) =
            let datum = CApi.NoScriptDatumForMint
                scriptData = CApi.ScriptDataConstructor 0 []
                redeemer = CApi.unsafeHashableScriptData scriptData
             in BuildTxWith $
                  CApi.PlutusScriptWitness
                    lang
                    PlutusScriptV3
                    (PScript plutusScript)
                    datum
                    redeemer
                    (CApi.ExecutionUnits 0 0) -- calculated during balancing
          quantity ∷ Quantity =
            1 -- NFT = only 1 token
         in
          case currentEra of
            ConwayEraOnwardsConway →
              TxMintValue CApi.MaryEraOnwardsConway $
                Map.singleton policy [(asset, quantity, wit)]

      bodyContent ∷ TxBodyContent BuildTx era =
        defaultTxBodyContent shelleyBasedEra
          & setTxIns
            [
              ( Utxo.utxoEntryInput utxoEntryForFee
              , BuildTxWith (KeyWitness KeyWitnessForSpending)
              )
            ]
          & setTxInsCollateral txInsCollateral
          & addTxOut scriptOutput
          & setTxMintValue txMintValue

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
      [witnessUtxoEntry utxoEntryForFee, witnessUtxoEntry utxoEntryForCollateral]

  modify' $ updateTagged @"submitted" (Set.insert (getTxId (getTxBody tx)))
  pure (policy, tx)

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

newtype Error = MintingTxError TxConstructionError
  deriving stock (Show)
  deriving anyclass (Exception)
