{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Yare.Chain.Tx
  ( Tx (..)
  , byEraByron
  , byEraShelley
  , byEraAllegra
  , byEraMary
  , byEraAlonzo
  , byEraBabbage
  , byEraConway
  , TxId
  , TxIn
  , TxOut (..)
  , TxViewUtxo (..)
  , TxOutViewUtxo (..)
  , blockTransactions
  , transactionViewUtxo
  ) where

import Relude

import Cardano.Api (TxId, TxIn)
import Cardano.Api qualified as CA
import Cardano.Api.Byron qualified as CAB
import Cardano.Api.Shelley
  ( Lovelace (..)
  , fromMaryValue
  , fromShelleyLovelace
  , lovelaceToValue
  )
import Cardano.Api.Shelley qualified as CAS
import Cardano.Chain.Block (ABlockOrBoundary (..), blockTxPayload)
import Cardano.Chain.Common (lovelaceToInteger)
import Cardano.Chain.UTxO (taTx, unTxPayload)
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Hashing qualified as ByronHashing
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody)
import Cardano.Ledger.Alonzo (AlonzoTxBody, AlonzoTxOut)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Api
  ( Addr (AddrBootstrap)
  , BootstrapAddress (..)
  , addrTxOutL
  , bodyTxL
  , inputsTxBodyL
  , outputsTxBodyL
  , valueTxOutL
  )
import Cardano.Ledger.Babbage (BabbageTxBody, BabbageTxOut)
import Cardano.Ledger.Block (bbody, txid)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody)
import Cardano.Ledger.Core (fromTxSeq)
import Cardano.Ledger.Mary (MaryTxBody)
import Cardano.Ledger.Shelley (ShelleyTx, ShelleyTxBody, ShelleyTxOut)
import Lens.Micro (folded, to, (^.), (^..))
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, byronBlockRaw)
import Ouroboros.Consensus.Cardano.Block
  ( HardForkBlock
      ( BlockAllegra
      , BlockAlonzo
      , BlockBabbage
      , BlockByron
      , BlockConway
      , BlockMary
      , BlockShelley
      )
  )
import Ouroboros.Consensus.Shelley.Eras
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
  , StandardCrypto
  )
import Ouroboros.Consensus.Shelley.Ledger.Block (shelleyBlockRaw)
import Yare.Chain.Block (HFBlock)
import Yare.Chain.Era
  ( Era (..)
  , EraFun (..)
  , IxedByEra (..)
  , NoIdx (..)
  , applyEraFun
  , fanout3EraFun
  , mapEraFun
  , runIxedByEra
  )
import Yare.Chain.Types (LedgerAddress)

-- Type family that maps an era to the corresponding Tx type.
type family TxForEra (era ∷ Era) where
  TxForEra Byron = Byron.Tx
  TxForEra Shelley = ShelleyTx (ShelleyEra StandardCrypto)
  TxForEra Allegra = ShelleyTx (AllegraEra StandardCrypto)
  TxForEra Mary = ShelleyTx (MaryEra StandardCrypto)
  TxForEra Alonzo = AlonzoTx (AlonzoEra StandardCrypto)
  TxForEra Babbage = AlonzoTx (BabbageEra StandardCrypto)
  TxForEra Conway = AlonzoTx (ConwayEra StandardCrypto)

-- Newtype wrapper around the TxForEra type family to allow partial application,
-- as type families cannot be partially applied.
newtype Tx (era ∷ Era) = Tx {unwrapTx ∷ TxForEra era}

byEraByron ∷ TxForEra Byron → IxedByEra Tx
byEraByron = IxedByEraByron . Tx

byEraShelley ∷ TxForEra Shelley → IxedByEra Tx
byEraShelley = IxedByEraShelley . Tx

byEraAllegra ∷ TxForEra Allegra → IxedByEra Tx
byEraAllegra = IxedByEraAllegra . Tx

byEraMary ∷ TxForEra Mary → IxedByEra Tx
byEraMary = IxedByEraMary . Tx

byEraAlonzo ∷ TxForEra Alonzo → IxedByEra Tx
byEraAlonzo = IxedByEraAlonzo . Tx

byEraBabbage ∷ TxForEra Babbage → IxedByEra Tx
byEraBabbage = IxedByEraBabbage . Tx

byEraConway ∷ TxForEra Conway → IxedByEra Tx
byEraConway = IxedByEraConway . Tx

--------------------------------------------------------------------------------
-- Tx body ---------------------------------------------------------------------

type family TxBodyForEra (era ∷ Era) where
  TxBodyForEra Byron = Byron.Tx
  TxBodyForEra Shelley = ShelleyTxBody (ShelleyEra StandardCrypto)
  TxBodyForEra Allegra = AllegraTxBody (AllegraEra StandardCrypto)
  TxBodyForEra Mary = MaryTxBody (MaryEra StandardCrypto)
  TxBodyForEra Alonzo = AlonzoTxBody (AlonzoEra StandardCrypto)
  TxBodyForEra Babbage = BabbageTxBody (BabbageEra StandardCrypto)
  TxBodyForEra Conway = ConwayTxBody (ConwayEra StandardCrypto)

newtype TxBody (era ∷ Era) = TxBody (TxBodyForEra era)

--------------------------------------------------------------------------------
-- Tx outputs ------------------------------------------------------------------

type family TxOutForEra (era ∷ Era) where
  TxOutForEra Byron = Byron.TxOut
  TxOutForEra Shelley = ShelleyTxOut (ShelleyEra StandardCrypto)
  TxOutForEra Allegra = ShelleyTxOut (AllegraEra StandardCrypto)
  TxOutForEra Mary = ShelleyTxOut (MaryEra StandardCrypto)
  TxOutForEra Alonzo = AlonzoTxOut (AlonzoEra StandardCrypto)
  TxOutForEra Babbage = BabbageTxOut (BabbageEra StandardCrypto)
  TxOutForEra Conway = BabbageTxOut (ConwayEra StandardCrypto)

data TxOut (era ∷ Era) = TxOut
  { txOutIndex ∷ CA.TxIx
  , unwrapTxOut ∷ TxOutForEra era
  }

--------------------------------------------------------------------------------
-- Tx views  -------------------------------------------------------------------

-- | Tx view for the purpose of tracking unspent tx outputs (UTxO)
data TxViewUtxo = TxViewUtxo
  { txViewId ∷ TxId
  , txViewInputs ∷ [TxIn]
  , txViewOutputs ∷ [TxOutViewUtxo]
  }
  deriving stock (Eq, Show)

-- | Tx output view for the purpose of tracking unspent tx outputs (UTxO)
data TxOutViewUtxo = TxOutViewUtxo
  { txOutViewUtxoIndex ∷ CA.TxIx
  , txOutViewUtxoAddress ∷ LedgerAddress
  , txOutViewUtxoValue ∷ CA.Value
  }
  deriving stock (Eq, Show)

transactionViewUtxo ∷ IxedByEra Tx → TxViewUtxo
transactionViewUtxo =
  runIxedByEra . applyEraFun (bodyEraFun >>> txViewUtxoEraFun)
 where
  txViewUtxoEraFun ∷ EraFun TxBody (NoIdx TxViewUtxo)
  txViewUtxoEraFun =
    fanout3EraFun
      idEraFun
      inputsEraFun
      (outputsEraFun >>> mapEraFun outputViewUtxoEraFun)
      \txId txInputs txOutputViews →
        NoIdx $
          TxViewUtxo
            (coerce txId)
            (coerce txInputs)
            (coerce txOutputViews)

  outputViewUtxoEraFun ∷ EraFun TxOut (NoIdx TxOutViewUtxo)
  outputViewUtxoEraFun =
    EraFun
      { eraFunByron = \(TxOut txOutIndex txOut) →
          NoIdx
            TxOutViewUtxo
              { txOutViewUtxoIndex = txOutIndex
              , txOutViewUtxoAddress =
                  AddrBootstrap (BootstrapAddress (Byron.txOutAddress txOut))
              , txOutViewUtxoValue =
                  lovelaceToValue
                    (Lovelace (lovelaceToInteger (Byron.txOutValue txOut)))
              }
      , eraFunShelley = mkTxOutViewUtxoSingleAsset
      , eraFunAllegra = mkTxOutViewUtxoSingleAsset
      , eraFunMary = mkTxOutViewUtxoMultiAsset
      , eraFunAlonzo = mkTxOutViewUtxoMultiAsset
      , eraFunBabbage = mkTxOutViewUtxoMultiAsset
      , eraFunConway = mkTxOutViewUtxoMultiAsset
      }
   where
    mkTxOutViewUtxoSingleAsset (TxOut txOutIndex txOut) =
      NoIdx
        TxOutViewUtxo
          { txOutViewUtxoIndex = txOutIndex
          , txOutViewUtxoAddress = txOut ^. addrTxOutL
          , txOutViewUtxoValue =
              lovelaceToValue (fromShelleyLovelace (txOut ^. valueTxOutL))
          }
    mkTxOutViewUtxoMultiAsset (TxOut txOutIndex txOut) =
      NoIdx
        TxOutViewUtxo
          { txOutViewUtxoIndex = txOutIndex
          , txOutViewUtxoAddress = txOut ^. addrTxOutL
          , txOutViewUtxoValue = fromMaryValue (txOut ^. valueTxOutL)
          }

--------------------------------------------------------------------------------
-- Functions -------------------------------------------------------------------

blockTransactions ∷ HFBlock → [IxedByEra Tx]
blockTransactions = \case
  BlockByron (byronBlock ∷ ByronBlock) →
    case byronBlockRaw byronBlock of
      ABOBBoundary _ → []
      ABOBBlock b → byEraByron . taTx <$> unTxPayload (blockTxPayload b)
  BlockShelley b →
    byEraShelley <$> toList (fromTxSeq (bbody (shelleyBlockRaw b)))
  BlockAllegra b →
    byEraAllegra <$> toList (fromTxSeq (bbody (shelleyBlockRaw b)))
  BlockMary b →
    byEraMary <$> toList (fromTxSeq (bbody (shelleyBlockRaw b)))
  BlockAlonzo b →
    byEraAlonzo <$> toList (fromTxSeq (bbody (shelleyBlockRaw b)))
  BlockBabbage b →
    byEraBabbage <$> toList (fromTxSeq (bbody (shelleyBlockRaw b)))
  BlockConway b →
    byEraConway <$> toList (fromTxSeq (bbody (shelleyBlockRaw b)))

bodyEraFun ∷ EraFun Tx TxBody
bodyEraFun =
  EraFun
    { eraFunByron = \(Tx tx) → TxBody tx
    , eraFunShelley = \(Tx tx) → TxBody (tx ^. bodyTxL)
    , eraFunAllegra = \(Tx tx) → TxBody (tx ^. bodyTxL)
    , eraFunMary = \(Tx tx) → TxBody (tx ^. bodyTxL)
    , eraFunAlonzo = \(Tx tx) → TxBody (tx ^. bodyTxL)
    , eraFunBabbage = \(Tx tx) → TxBody (tx ^. bodyTxL)
    , eraFunConway = \(Tx tx) → TxBody (tx ^. bodyTxL)
    }

idEraFun ∷ EraFun TxBody (NoIdx TxId)
idEraFun =
  EraFun
    { eraFunByron = \(TxBody tx) → do
        let txId = ByronHashing.serializeCborHash tx
            shortHash = ByronHashing.abstractHashToShort txId
        case Crypto.hashFromBytesShort shortHash of
          Just apiHash → NoIdx $ CA.TxId apiHash
          Nothing → error $ "Error converting Byron era TxId: " <> show txId
    , eraFunShelley = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txid tx
    , eraFunAllegra = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txid tx
    , eraFunMary = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txid tx
    , eraFunAlonzo = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txid tx
    , eraFunBabbage = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txid tx
    , eraFunConway = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txid tx
    }

inputsEraFun ∷ EraFun TxBody (Compose [] (NoIdx TxIn))
inputsEraFun =
  EraFun
    { eraFunByron = \(TxBody tx) →
        Compose $
          tx ^.. to Byron.txInputs . folded . to (NoIdx . CAB.fromByronTxIn)
    , eraFunShelley = \(TxBody tx) →
        Compose $
          tx ^.. inputsTxBodyL . folded . to (NoIdx . CAS.fromShelleyTxIn)
    , eraFunAllegra = \(TxBody tx) →
        Compose $
          tx ^.. inputsTxBodyL . folded . to (NoIdx . CAS.fromShelleyTxIn)
    , eraFunMary = \(TxBody tx) →
        Compose $
          tx ^.. inputsTxBodyL . folded . to (NoIdx . CAS.fromShelleyTxIn)
    , eraFunAlonzo = \(TxBody tx) →
        Compose $
          tx ^.. inputsTxBodyL . folded . to (NoIdx . CAS.fromShelleyTxIn)
    , eraFunBabbage = \(TxBody tx) →
        Compose $
          tx ^.. inputsTxBodyL . folded . to (NoIdx . CAS.fromShelleyTxIn)
    , eraFunConway = \(TxBody tx) →
        Compose $
          tx ^.. inputsTxBodyL . folded . to (NoIdx . CAS.fromShelleyTxIn)
    }

outputsEraFun ∷ EraFun TxBody (Compose [] TxOut)
outputsEraFun =
  EraFun
    { eraFunByron = \(TxBody tx) → Compose $ mkTxOut $ Byron.txOutputs tx
    , eraFunShelley = \(TxBody tx) → Compose $ mkTxOut $ tx ^. outputsTxBodyL
    , eraFunAllegra = \(TxBody tx) → Compose $ mkTxOut $ tx ^. outputsTxBodyL
    , eraFunMary = \(TxBody tx) → Compose $ mkTxOut $ tx ^. outputsTxBodyL
    , eraFunAlonzo = \(TxBody tx) → Compose $ mkTxOut $ tx ^. outputsTxBodyL
    , eraFunBabbage = \(TxBody tx) → Compose $ mkTxOut $ tx ^. outputsTxBodyL
    , eraFunConway = \(TxBody tx) → Compose $ mkTxOut $ tx ^. outputsTxBodyL
    }
 where
  mkTxOut ∷ Foldable f ⇒ f (TxOutForEra era) → [TxOut era]
  mkTxOut = zipWith TxOut [CA.TxIx 0 ..] . toList
