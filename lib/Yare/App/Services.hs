module Yare.App.Services
  ( Services (..)
  , mkServices
  ) where

import Relude

import Cardano.Api (TxInsReference (..))
import Cardano.Api.Ledger (Credential, KeyRole (DRepRole))
import Cardano.Api.Shelley
  ( AlonzoEraOnwards (..)
  , BuildTx
  , BuildTxWith (..)
  , CtxTx
  , InAnyShelleyBasedEra (..)
  , KeyWitnessInCtx (..)
  , PoolId
  , ShelleyBasedEra (..)
  , ShelleyWitnessSigningKey
  , StakeCredential
  , Tx
  , TxBodyContent (..)
  , TxBodyErrorAutoBalance
  , TxInMode (..)
  , TxInsCollateral (..)
  , TxOut (..)
  , UTxO
  , Witness (..)
  , constructBalancedTx
  , fromShelleyAddrIsSbe
  , inAnyShelleyBasedEra
  )
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Lens (Lens, Simple, lens, set, view, (.~), (^.), _1)
import Control.Monad.Oops (Variant)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Variant qualified as Variant
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr)
import Yare.Addresses (Addresses)
import Yare.Addresses qualified as Addresses
import Yare.App.Types (AppState, NetworkInfo (..), addressState, chainState)
import Yare.Chain.Follower (ChainState (..), chainTip, utxoState)
import Yare.Chain.Types (ChainTip)
import Yare.Funds qualified as Funds
import Yare.Storage (Storage (..), overStorageState, stateful, stateful', zoomStorage)
import Yare.Submitter qualified as Submitter
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo
import Yare.Utxo.State (UtxoState, spendableUtxoEntries)

-- | Application services
type Services ∷ (Type → Type) → Type
data Services m = Services
  { serveUtxo ∷ m Utxo
  , serveTip ∷ m ChainTip
  , deployScript
      ∷ IO
          ( Maybe
              ( Variant
                  [ CardanoApplyTxErr StandardCrypto
                  , InAnyShelleyBasedEra TxBodyErrorAutoBalance
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
    { serveUtxo =
        serveUtxoFromStorage chainStateStorage
    , serveTip =
        serveTipFromStorage chainStateStorage
    , deployScript =
        serviceDeployScript
          (zoomStorage (both addressState (chainState . utxoState)) storage)
          submitQ
          networkInfo
    }
 where
  chainStateStorage = zoomStorage chainState storage

-- | Retrieves the UTXO set from a storage.
serveUtxoFromStorage ∷ Storage IO ChainState → IO Utxo
serveUtxoFromStorage storage = do
  s ← readStorage storage
  pure $ Utxo.fromList (Map.toList (spendableUtxoEntries (s ^. utxoState)))

-- | Retrieves the chain tip from a storage.
serveTipFromStorage ∷ Storage IO ChainState → IO ChainTip
serveTipFromStorage storage = view chainTip <$> readStorage storage

-- | Deploys a script on-chain by submitting a transaction.
serviceDeployScript
  ∷ ∀ era
   . Storage IO (Addresses, UtxoState)
  → Submitter.Q
  → NetworkInfo era
  → IO
      ( Maybe
          ( Variant
              [ CardanoApplyTxErr StandardCrypto
              , InAnyShelleyBasedEra TxBodyErrorAutoBalance
              ]
          )
      )
serviceDeployScript storage submitQ networkInfo = do
  let NetworkInfo {currentEra} = networkInfo
  overStorageState storage (txDeployScript networkInfo) >>= \case
    Left err →
      pure $ Just $ Variant.throw $ inAnyShelleyBasedEra currentEra err
    Right signedBalancedTx →
      Submitter.submit submitQ (TxInMode currentEra signedBalancedTx)

txDeployScript
  ∷ ∀ era
   . NetworkInfo era
  → State
      (Addresses, UtxoState)
      (Either (TxBodyErrorAutoBalance era) (Tx era))
txDeployScript networkInfo = do
  let NetworkInfo
        { protocolParameters
        , epochInfo
        , systemStart
        , currentEra
        } = networkInfo
  originalState ← get
  feeInputs ← stateful Funds.useFeeInputs
  colInputs ← stateful Funds.useCollateralInputs
  changeAddr ←
    fromShelleyAddrIsSbe currentEra <$> stateful' _1 Addresses.useChangeAddress

  let
    scriptOutput ∷ TxOut CtxTx era
    scriptOutput = undefined $ error "not implemented"

    {-

      data TxOut = TxOut
          (AddressInEra era)
          (TxOutValue era)
          (TxOutDatum ctx era)
          (ReferenceScript era)

     TxBodyContent {
       txIns                :: [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))],
       txInsCollateral      :: TxInsCollateral era,
       txInsReference       :: TxInsReference build era,
       txOuts               :: [TxOut CtxTx era],
       txTotalCollateral    :: TxTotalCollateral era,
       txReturnCollateral   :: TxReturnCollateral CtxTx era,
       txFee                :: TxFee era,
       txValidityLowerBound :: TxValidityLowerBound era,
       txValidityUpperBound :: TxValidityUpperBound era,
       txMetadata           :: TxMetadataInEra era,
       txAuxScripts         :: TxAuxScripts era,
       txExtraKeyWits       :: TxExtraKeyWitnesses era,
       txProtocolParams     :: BuildTxWith build (Maybe (LedgerProtocolParameters era)),
       txWithdrawals        :: TxWithdrawals  build era,
       txCertificates       :: TxCertificates build era,
       txUpdateProposal     :: TxUpdateProposal era,
       txMintValue          :: TxMintValue    build era,
       txScriptValidity     :: TxScriptValidity era,
       txProposalProcedures :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era)),
       txVotingProcedures   :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era)),
       -- | Current treasury value
       txCurrentTreasuryValue :: Maybe (Featured ConwayEraOnwards era L.Coin),
       -- | Treasury donation to perform
       txTreasuryDonation     :: Maybe (Featured ConwayEraOnwards era L.Coin)
     }
    -}

    bodyContent ∷ TxBodyContent BuildTx era
    bodyContent =
      TxBodyContent
        { txIns =
            (,BuildTxWith (KeyWitness KeyWitnessForSpending))
              <$> toList feeInputs
        , txInsCollateral =
            let
              collInputs ∷ AlonzoEraOnwards era → TxInsCollateral era
              collInputs = (`TxInsCollateral` toList colInputs)
             in
              case currentEra of
                ShelleyBasedEraShelley → TxInsCollateralNone
                ShelleyBasedEraAllegra → TxInsCollateralNone
                ShelleyBasedEraMary → TxInsCollateralNone
                ShelleyBasedEraAlonzo → collInputs AlonzoEraOnwardsAlonzo
                ShelleyBasedEraBabbage → collInputs AlonzoEraOnwardsBabbage
                ShelleyBasedEraConway → collInputs AlonzoEraOnwardsConway
        , txInsReference = TxInsReferenceNone
        , txOuts = [scriptOutput]
        , txTotalCollateral = undefined $ error "not implemented"
        , txReturnCollateral = undefined $ error "not implemented"
        , txFee = undefined $ error "not implemented"
        , txValidityLowerBound = undefined $ error "not implemented"
        , txValidityUpperBound = undefined $ error "not implemented"
        , txMetadata = undefined $ error "not implemented"
        , txAuxScripts = undefined $ error "not implemented"
        , txExtraKeyWits = undefined $ error "not implemented"
        , txProtocolParams = undefined $ error "not implemented"
        , txWithdrawals = undefined $ error "not implemented"
        , txCertificates = undefined $ error "not implemented"
        , txUpdateProposal = undefined $ error "not implemented"
        , txMintValue = undefined $ error "not implemented"
        , txScriptValidity = undefined $ error "not implemented"
        , txProposalProcedures = undefined $ error "not implemented"
        , txVotingProcedures = undefined $ error "not implemented"
        , txCurrentTreasuryValue = undefined $ error "not implemented"
        , txTreasuryDonation = undefined $ error "not implemented"
        }

    overrideKeyWitnesses ∷ Maybe Word
    overrideKeyWitnesses = Nothing

    txInputs ∷ UTxO era
    txInputs = undefined $ error "not implemented"

    registeredPools ∷ Set PoolId
    registeredPools = Set.empty

    delegations ∷ Map StakeCredential L.Coin
    delegations = Map.empty

    delegationsRewards ∷ Map (Credential DRepRole StandardCrypto) L.Coin
    delegationsRewards = Map.empty

    witnesses ∷ [ShelleyWitnessSigningKey]
    witnesses = []

    txConstructionResult =
      constructBalancedTx
        currentEra
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

  when (isLeft txConstructionResult) do
    put originalState

  pure txConstructionResult

--------------------------------------------------------------------------------
-- Lens utilities --------------------------------------------------------------

both ∷ Simple Lens s a → Simple Lens s b → Simple Lens s (a, b)
both l r =
  lens
    (\s → (s ^. l, s ^. r))
    (\s (a, b) → s & l .~ a & r .~ b)
