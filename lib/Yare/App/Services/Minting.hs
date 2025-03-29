module Yare.App.Services.Minting where

import Yare.Prelude hiding (show)

import Cardano.Api.Ledger (Credential, KeyRole (DRepRole))
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , AssetId (..)
  , AssetName
  , BabbageEraOnwards
  , BuildTx
  , BuildTxWith (BuildTxWith)
  , ConwayEraOnwards (..)
  , CtxTx
  , ExecutionUnits (..)
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , LedgerEpochInfo
  , LedgerProtocolParameters
  , Lovelace
  , MaryEraOnwards (..)
  , PlutusScriptOrReferenceInput (PScript)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , PolicyId (..)
  , PoolId
  , Quantity (..)
  , ReferenceScript (..)
  , ScriptData (ScriptDataConstructor)
  , ScriptDatum (NoScriptDatumForMint)
  , ScriptLanguageInEra (..)
  , ScriptWitness (PlutusScriptWitness)
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
  , TxOutValue (TxOutValueShelleyBased)
  , Value
  , WitCtxMint
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
  , setTxProtocolParams
  , shelleyBasedEraConstraints
  , toLedgerValue
  , unsafeHashableScriptData
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Exception (throwIO)
import Control.Monad.Except (Except, throwError)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Debug.Trace qualified as Debug
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
  let network ∷ NetworkInfo era = look env
      protocolParams ∷ LedgerProtocolParameters era = protocolParameters network
      era ∷ ConwayEraOnwards era = currentEra network
      epoch ∷ LedgerEpochInfo = epochInfo network
      addresses ∷ Addresses = look @Addresses env
      shelleyBasedEra ∷ ShelleyBasedEra era = convert era
      babbageEraOnwards ∷ BabbageEraOnwards era = convert era
      maryEraOnwards ∷ MaryEraOnwards era = convert babbageEraOnwards

  utxoEntryForCollateral@Utxo.MkEntry {utxoEntryAddress = collateralAddress} ←
    usingMonadState (Utxo.useInputCollateral addresses)
      >>= maybe (throwError (MintingTxError NoCollateralInputs)) pure

  Utxo.MkEntry {utxoEntryInput = singletonInput} ←
    usingMonadState (Utxo.useInputLowestAdaOnly addresses (0 ∷ Lovelace))
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

  let (plutusScript, PolicyId → policy) =
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

  let (scriptOutputAda ∷ Lovelace, scriptOutput ∷ TxOut CtxTx era) =
        let
          val ∷ Value = fromList [(AssetId policy asset, Quantity 1)]
          txOutValue ∷ TxOutValue era =
            shelleyBasedEraConstraints shelleyBasedEra $
              TxOutValueShelleyBased shelleyBasedEra $
                toLedgerValue maryEraOnwards val
          address ∷ LedgerAddress =
            ledgerAddress (Address.useForMinting addresses)
         in
          mkScriptOutput
            shelleyBasedEra
            protocolParams
            address
            txOutValue
            TxOutDatumNone
            ReferenceScriptNone

  -- The fee entry should contain enough ADA to cover the minimum UTxO value
  -- of the script output. Otherwise the 'createBalancedTx' function will fail
  -- with an "Negative value" error.
  utxoEntryForFee ∷ Utxo.Entry ←
    usingMonadState
      (Utxo.useInputFee addresses {- not less than: -} scriptOutputAda)
      >>= maybe (throwError (MintingTxError NoFeeInputs)) pure

  let
    wrapError =
      MintingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

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

      txInsCollateral ∷ TxInsCollateral era =
        case era of
          ConwayEraOnwardsConway →
            TxInsCollateral
              AlonzoEraOnwardsConway
              [Utxo.utxoEntryInput utxoEntryForCollateral]

      txMintValue ∷ TxMintValue BuildTx era =
        let
          lang ∷ ScriptLanguageInEra PlutusScriptV3 era =
            case era of ConwayEraOnwardsConway → PlutusScriptV3InConway

          wit ∷ BuildTxWith BuildTx (ScriptWitness WitCtxMint era) =
            let datum = NoScriptDatumForMint
                scriptData = ScriptDataConstructor 0 []
                redeemer = unsafeHashableScriptData scriptData
             in BuildTxWith $
                  PlutusScriptWitness
                    lang
                    PlutusScriptV3
                    (PScript plutusScript)
                    datum
                    redeemer
                    (ExecutionUnits 0 0) -- calculated during balancing
          quantity = Quantity 1 -- NFT = only 1 token
         in
          case era of
            ConwayEraOnwardsConway →
              TxMintValue MaryEraOnwardsConway $
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
          & setTxProtocolParams (BuildTxWith (Just protocolParams))

    Debug.traceM (toString (pShow (mkCardanoApiUtxo era [utxoEntryForFee])))

    constructBalancedTx
      shelleyBasedEra
      bodyContent
      changeAddress
      overrideKeyWitnesses
      (mkCardanoApiUtxo era [utxoEntryForFee])
      protocolParams
      epoch
      (systemStart network)
      registeredPools
      delegations
      rewards
      [ witnessUtxoEntry utxoEntryForFee
      , witnessUtxoEntry utxoEntryForCollateral
      ]

  modify' $ updateTagged @"submitted" (Set.insert (getTxId (getTxBody tx)))
  pure (policy, tx)

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

newtype Error = MintingTxError TxConstructionError
  deriving stock (Show)
  deriving anyclass (Exception)
