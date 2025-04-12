module Yare.App.Services.Minting where

import Yare.Prelude hiding (show)

import Cardano.Api.Shelley
  ( AddressInEra
  , AlonzoEraOnwards (..)
  , AssetId (..)
  , AssetName
  , BabbageEraOnwards
  , BuildTx
  , BuildTxWith (BuildTxWith)
  , ConwayEra
  , ConwayEraOnwards (..)
  , CtxTx
  , ExecutionUnits (..)
  , Hash
  , KeyWitnessInCtx (KeyWitnessForSpending)
  , LedgerEpochInfo
  , LedgerProtocolParameters (..)
  , Lovelace
  , MaryEraOnwards (..)
  , PaymentKey
  , PlutusScriptOrReferenceInput (PScript)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , PolicyId (..)
  , Quantity (..)
  , ReferenceScript (..)
  , ScriptData (ScriptDataConstructor)
  , ScriptDatum (NoScriptDatumForMint)
  , ScriptLanguageInEra (..)
  , ScriptWitness (PlutusScriptWitness)
  , ShelleyBasedEra
  , ShelleyLedgerEra
  , Tx (..)
  , TxBodyContent (..)
  , TxExtraKeyWitnesses (..)
  , TxId
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxMintValue (..)
  , TxOut (..)
  , TxOutDatum (..)
  , TxOutValue
  , UTxO
  , WitCtxMint
  , Witness (KeyWitness)
  , addTxOut
  , calculateMinimumUTxO
  , constructBalancedTx
  , convert
  , defaultTxBodyContent
  , fromShelleyAddrIsSbe
  , getTxBody
  , getTxId
  , inAnyShelleyBasedEra
  , lovelaceToTxOutValue
  , runExcept
  , setTxExtraKeyWits
  , setTxIns
  , setTxInsCollateral
  , setTxMintValue
  , setTxProtocolParams
  , toLedgerValue
  , unsafeHashableScriptData
  )
import Cardano.Ledger.Api qualified as Ledger
import Control.Exception (throwIO)
import Control.Monad.Except (Except, throwError)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Text.Pretty.Simple (pShow)
import Unsafe.Coerce (unsafeCoerce)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address.Derivation (AddressWithKey (..))
import Yare.App.Scripts.MintingPolicy qualified as MintingPolicy
import Yare.App.Services.Error (TxConstructionError (..))
import Yare.App.Types (NetworkInfo (..), StorageMode (..))
import Yare.Chain.Types (ledgerAddressPaymentKeyHash)
import Yare.Compat.Plutus (paymentKeyHashToPubKeyHash, txOutRefFromTxIn)
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
    Left err → do
      putTextLn "Error while minting:"
      putTextLn . toStrict $ pShow err
      throwIO err
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
    network ∷ NetworkInfo era = look env
    protocolParams ∷ Ledger.PParams (ShelleyLedgerEra era) =
      protocolParameters network
    era ∷ ConwayEraOnwards era = currentEra network
    epoch ∷ LedgerEpochInfo = epochInfo network
    addresses ∷ Addresses = look @Addresses env
    shelleyBasedEra ∷ ShelleyBasedEra era = convert era
    babbageOnwards ∷ BabbageEraOnwards era = convert era
    alonzoOnwards ∷ AlonzoEraOnwards era = convert babbageOnwards
    maryOnwards ∷ MaryEraOnwards era = convert babbageOnwards

    changeAddress ∷ AddressInEra era =
      fromShelleyAddrIsSbe shelleyBasedEra . ledgerAddress $
        Address.useForChange addresses

    -- The minimum UTxO ADA value is calculated for a dummy tx output. This is
    -- done to avoid an error when the [collateral] change output is created by
    -- the 'constructBalancedTx' function with a value that is less than the
    -- minimum UTxO value.
    minUtxoAda ∷ Lovelace =
      calculateMinimumUTxO shelleyBasedEra dummyTxOut protocolParams
     where
      dummyTxOut ∷ TxOut CtxTx era =
        TxOut changeAddress dummyBalance TxOutDatumNone ReferenceScriptNone
      dummyBalance ∷ TxOutValue era =
        lovelaceToTxOutValue shelleyBasedEra 10_000_000

    expectedTotalCollateral ∷ Lovelace = 600_000 -- found empirically
    minCollateralAdaValue ∷ Lovelace = expectedTotalCollateral + minUtxoAda
  utxoEntryForCollateral@Utxo.MkEntry {utxoEntryAddress = collateralAddress} ←
    usingMonadState (Utxo.useInputCollateral addresses minCollateralAdaValue)
      >>= maybe (throwError (MintingTxError NoCollateralInputs)) pure

  utxoEntryForSingleton@Utxo.MkEntry {utxoEntryInput = singletonInput} ←
    usingMonadState (Utxo.useInputLowestAdaOnly addresses minUtxoAda)
      >>= maybe (throwError (MintingTxError NoSingletonInputs)) pure

  whoCanMint ∷ Hash PaymentKey ←
    -- Minting policy only allows to mint if Tx is signed by the owner of the
    -- public key given to it as a configuration parameter. In order not to
    -- add extra signature to the minting Tx (thus increasing its size) we use
    -- public key hash of the collateral address as its signature is added to
    -- the minting Tx anyway.
    case ledgerAddressPaymentKeyHash collateralAddress of
      Nothing → throwError (MintingTxError ScriptAddressNoPublicKeyHash)
      Just pkh → pure pkh

  let (plutusScript, PolicyId → policy) =
        MintingPolicy.serialised
          MintingPolicy.MkMintingParams
            { whoCanMint = paymentKeyHashToPubKeyHash whoCanMint
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

  let
    (scriptOutputAda ∷ Lovelace, scriptOutput ∷ TxOut CtxTx era) =
      let
        value = fromList [(AssetId policy asset, Quantity 1)]
        address = ledgerAddress (Address.useForMinting addresses)
       in
        mkScriptOutput
          shelleyBasedEra
          (LedgerProtocolParameters protocolParams)
          address
          (toLedgerValue maryOnwards value)
          TxOutDatumNone
          ReferenceScriptNone

  utxoEntryForFee ∷ Utxo.Entry ← do
    let
      -- The fee value should cover:
      -- 1. Expected total fee of the transaction.
      -- 2. Minimum ADA value of the change output.
      -- 3. Minimum ADA value of the script output.
      expectedTotalFee ∷ Lovelace = 500_000 -- found empirically
      minFeeAdaValue ∷ Lovelace =
        expectedTotalFee + minUtxoAda + scriptOutputAda
    usingMonadState
      (Utxo.useInputFee addresses {- not less than: -} minFeeAdaValue)
      >>= maybe (throwError (MintingTxError NoFeeInputs)) pure

  let
    wrapError =
      MintingTxError
        . TxAutoBalanceError
        . inAnyShelleyBasedEra shelleyBasedEra

  tx ← either (throwError . wrapError) pure do
    let
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
            ,
              ( singletonInput
              , BuildTxWith (KeyWitness KeyWitnessForSpending)
              )
            ]
          & setTxInsCollateral txInsCollateral
          & addTxOut scriptOutput
          & setTxMintValue txMintValue
          & setTxExtraKeyWits
            (TxExtraKeyWitnesses alonzoOnwards [whoCanMint])
          & setTxProtocolParams
            (BuildTxWith (Just (LedgerProtocolParameters protocolParams)))

    let inputsForBalancing ∷ UTxO era =
          mkCardanoApiUtxo
            era
            [ utxoEntryForFee
            , utxoEntryForCollateral
            , utxoEntryForSingleton
            ]

    traceM . toString $
      pShow (unsafeCoerce bodyContent ∷ TxBodyContent BuildTx ConwayEra)

    traceM . toString $
      pShow inputsForBalancing

    constructBalancedTx
      shelleyBasedEra
      bodyContent
      changeAddress
      empty {- overrideKeyWitnesses -}
      inputsForBalancing
      (LedgerProtocolParameters protocolParams)
      epoch
      (systemStart network)
      mempty {- registered pools -}
      mempty {- delegations      -}
      mempty {- rewards          -}
      [ witnessUtxoEntry utxoEntryForFee
      , witnessUtxoEntry utxoEntryForCollateral
      , witnessUtxoEntry utxoEntryForSingleton
      ]

  modify' $ updateTagged @"submitted" (Set.insert (getTxId (getTxBody tx)))
  pure (policy, tx)

--------------------------------------------------------------------------------
-- Error types -----------------------------------------------------------------

newtype Error = MintingTxError TxConstructionError
  deriving stock (Show)
  deriving anyclass (Exception)
