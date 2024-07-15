{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Yare.Chain.Tx
  ( Tx (..)
  , toConsensusGenTx
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
import Cardano.Api.Shelley (fromMaryValue, lovelaceToValue)
import Cardano.Api.Shelley qualified as CAS
import Cardano.Chain.Block (ABlockOrBoundary (..), blockTxPayload)
import Cardano.Chain.Common (unsafeGetLovelace)
import Cardano.Chain.UTxO (unTxPayload)
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Hashing (serializeCborHash)
import Cardano.Crypto.Hashing qualified as ByronHashing
import Cardano.Ledger.Address (decompactAddr)
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody)
import Cardano.Ledger.Alonzo (AlonzoTxBody, AlonzoTxOut)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Api
  ( Addr (AddrBootstrap)
  , BootstrapAddress (..)
  , bodyTxL
  , inputsTxBodyL
  , outputsTxBodyL
  , valueTxOutL
  )
import Cardano.Ledger.Babbage (BabbageTxBody, BabbageTxOut)
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Coin (word64ToCoin)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody)
import Cardano.Ledger.Core (addrEitherTxOutL, coinTxOutL, fromTxSeq)
import Cardano.Ledger.Mary (MaryTxBody)
import Cardano.Ledger.Shelley (ShelleyTx, ShelleyTxBody, ShelleyTxOut)
import Cardano.Ledger.Shelley.Core (txIdTxBody)
import Cardano.Ledger.Shelley.TxOut (addrEitherShelleyTxOutL)
import Data.SOP.Strict (NS (S, Z))
import Lens.Micro (folded, to, (^.), (^..))
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, byronBlockRaw)
import Ouroboros.Consensus.Byron.Ledger qualified as Consensus
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
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
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
import Ouroboros.Consensus.Shelley.Ledger.Mempool qualified as CS
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Era
  ( AnyEra (..)
  , Era (..)
  , EraFun (..)
  , NoIdx (..)
  , applyEraFun
  , fanout3EraFun
  , mapEraFun
  , runInEra
  )
import Yare.Chain.Types (LedgerAddress)

-- Type family that maps an era to the corresponding Tx type.
type TxInEra ∷ Era → Type
type family TxInEra era ∷ Type where
  TxInEra Byron = Byron.TxAux
  TxInEra Shelley = ShelleyTx (ShelleyEra StandardCrypto)
  TxInEra Allegra = ShelleyTx (AllegraEra StandardCrypto)
  TxInEra Mary = ShelleyTx (MaryEra StandardCrypto)
  TxInEra Alonzo = AlonzoTx (AlonzoEra StandardCrypto)
  TxInEra Babbage = AlonzoTx (BabbageEra StandardCrypto)
  TxInEra Conway = AlonzoTx (ConwayEra StandardCrypto)

-- Newtype wrapper around the TxInEra type family to allow partial application,
-- as type families cannot be partially applied.
type Tx ∷ Era → Type
newtype Tx era = Tx {unwrapTx ∷ TxInEra era}

byEraByron ∷ TxInEra Byron → AnyEra Tx
byEraByron = InEraByron . Tx

byEraShelley ∷ TxInEra Shelley → AnyEra Tx
byEraShelley = InEraShelley . Tx

byEraAllegra ∷ TxInEra Allegra → AnyEra Tx
byEraAllegra = InEraAllegra . Tx

byEraMary ∷ TxInEra Mary → AnyEra Tx
byEraMary = InEraMary . Tx

byEraAlonzo ∷ TxInEra Alonzo → AnyEra Tx
byEraAlonzo = InEraAlonzo . Tx

byEraBabbage ∷ TxInEra Babbage → AnyEra Tx
byEraBabbage = InEraBabbage . Tx

byEraConway ∷ TxInEra Conway → AnyEra Tx
byEraConway = InEraConway . Tx

--------------------------------------------------------------------------------
-- Tx body ---------------------------------------------------------------------

type TxBodyForEra ∷ Era → Type
type family TxBodyForEra era ∷ Type where
  TxBodyForEra Byron = Byron.TxAux
  TxBodyForEra Shelley = ShelleyTxBody (ShelleyEra StandardCrypto)
  TxBodyForEra Allegra = AllegraTxBody (AllegraEra StandardCrypto)
  TxBodyForEra Mary = MaryTxBody (MaryEra StandardCrypto)
  TxBodyForEra Alonzo = AlonzoTxBody (AlonzoEra StandardCrypto)
  TxBodyForEra Babbage = BabbageTxBody (BabbageEra StandardCrypto)
  TxBodyForEra Conway = ConwayTxBody (ConwayEra StandardCrypto)

type TxBody ∷ Era → Type
newtype TxBody era = TxBody (TxBodyForEra era)

--------------------------------------------------------------------------------
-- Tx outputs ------------------------------------------------------------------

type TxOutForEra ∷ Era → Type
type family TxOutForEra era ∷ Type where
  TxOutForEra Byron = Byron.TxOut
  TxOutForEra Shelley = ShelleyTxOut (ShelleyEra StandardCrypto)
  TxOutForEra Allegra = ShelleyTxOut (AllegraEra StandardCrypto)
  TxOutForEra Mary = ShelleyTxOut (MaryEra StandardCrypto)
  TxOutForEra Alonzo = AlonzoTxOut (AlonzoEra StandardCrypto)
  TxOutForEra Babbage = BabbageTxOut (BabbageEra StandardCrypto)
  TxOutForEra Conway = BabbageTxOut (ConwayEra StandardCrypto)

type TxOut ∷ Era → Type
data TxOut era = TxOut
  { txOutIndex ∷ CA.TxIx
  , unwrapTxOut ∷ TxOutForEra era
  }

--------------------------------------------------------------------------------
-- Tx views  -------------------------------------------------------------------

-- | Tx view for the purpose of tracking unspent tx outputs (UTxO)
type TxViewUtxo ∷ Type
data TxViewUtxo = TxViewUtxo
  { txViewId ∷ TxId
  , txViewInputs ∷ [TxIn]
  , txViewOutputs ∷ [TxOutViewUtxo]
  }
  deriving stock (Eq, Show)

-- | Tx output view for the purpose of tracking unspent tx outputs (UTxO)
type TxOutViewUtxo ∷ Type
data TxOutViewUtxo = TxOutViewUtxo
  { txOutViewUtxoIndex ∷ CA.TxIx
  , txOutViewUtxoAddress ∷ LedgerAddress
  , txOutViewUtxoValue ∷ CA.Value
  }
  deriving stock (Eq, Show)

transactionViewUtxo ∷ AnyEra Tx → TxViewUtxo
transactionViewUtxo =
  runInEra . applyEraFun (bodyEraFun >>> txViewUtxoEraFun)
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
                    (word64ToCoin (unsafeGetLovelace (Byron.txOutValue txOut)))
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
          , txOutViewUtxoAddress =
              case txOut ^. addrEitherShelleyTxOutL of
                Left addr → addr
                Right compactAddr → decompactAddr compactAddr
          , txOutViewUtxoValue =
              lovelaceToValue (txOut ^. coinTxOutL)
          }
    mkTxOutViewUtxoMultiAsset (TxOut txOutIndex txOut) =
      NoIdx
        TxOutViewUtxo
          { txOutViewUtxoIndex = txOutIndex
          , txOutViewUtxoAddress =
              case txOut ^. addrEitherTxOutL of
                Left addr → addr
                Right compactAddr → decompactAddr compactAddr
          , txOutViewUtxoValue = fromMaryValue (txOut ^. valueTxOutL)
          }

--------------------------------------------------------------------------------
-- Functions -------------------------------------------------------------------

blockTransactions ∷ StdCardanoBlock → [AnyEra Tx]
blockTransactions = \case
  BlockByron (byronBlock ∷ ByronBlock) →
    case byronBlockRaw byronBlock of
      ABOBBoundary _ → []
      ABOBBlock b → byEraByron <$> unTxPayload (blockTxPayload b)
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
    , eraFunShelley = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txIdTxBody tx
    , eraFunAllegra = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txIdTxBody tx
    , eraFunMary = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txIdTxBody tx
    , eraFunAlonzo = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txIdTxBody tx
    , eraFunBabbage = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txIdTxBody tx
    , eraFunConway = \(TxBody tx) → NoIdx . CAS.fromShelleyTxId $ txIdTxBody tx
    }

inputsEraFun ∷ EraFun TxBody (Compose [] (NoIdx TxIn))
inputsEraFun =
  EraFun
    { eraFunByron = \(TxBody tx) →
        Compose $
          Byron.taTx tx
            ^.. to Byron.txInputs
            . folded
            . to (NoIdx . CAB.fromByronTxIn)
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
    { eraFunByron = \(TxBody tx) → mkTxOut $ Byron.txOutputs $ Byron.taTx tx
    , eraFunShelley = \(TxBody tx) → mkTxOut $ tx ^. outputsTxBodyL
    , eraFunAllegra = \(TxBody tx) → mkTxOut $ tx ^. outputsTxBodyL
    , eraFunMary = \(TxBody tx) → mkTxOut $ tx ^. outputsTxBodyL
    , eraFunAlonzo = \(TxBody tx) → mkTxOut $ tx ^. outputsTxBodyL
    , eraFunBabbage = \(TxBody tx) → mkTxOut $ tx ^. outputsTxBodyL
    , eraFunConway = \(TxBody tx) → mkTxOut $ tx ^. outputsTxBodyL
    }
 where
  mkTxOut ∷ Foldable f ⇒ f (TxOutForEra era) → Compose [] TxOut era
  mkTxOut = Compose . zipWith TxOut [CA.TxIx 0 ..] . toList

--------------------------------------------------------------------------------
-- Consensus conversions -------------------------------------------------------

type ConsensusTx ∷ Type
type ConsensusTx = Consensus.GenTx StdCardanoBlock

toConsensusGenTx ∷ AnyEra Tx → ConsensusTx
toConsensusGenTx anyEraTx = runInEra (applyEraFun EraFun {..} anyEraTx)
 where
  eraFunByron ∷ Tx Byron → NoIdx ConsensusTx x
  eraFunByron (Tx txAux) = mkGenTx Z genTx
   where
    genTx ∷ Consensus.GenTx Consensus.ByronBlock
    genTx = Consensus.ByronTx txId (Byron.annotateTxAux txAux)
     where
      txId ∷ Byron.TxId
      txId = serializeCborHash (Byron.taTx txAux)

  eraFunShelley ∷ Tx Shelley → NoIdx ConsensusTx x
  eraFunShelley = mkConsensusTx (S . Z)

  eraFunAllegra ∷ Tx Allegra → NoIdx ConsensusTx x
  eraFunAllegra = mkConsensusTx (S . S . Z)

  eraFunMary ∷ Tx Mary → NoIdx ConsensusTx x
  eraFunMary = mkConsensusTx (S . S . S . Z)

  eraFunAlonzo ∷ Tx Alonzo → NoIdx ConsensusTx x
  eraFunAlonzo = mkConsensusTx (S . S . S . S . Z)

  eraFunBabbage ∷ Tx Babbage → NoIdx ConsensusTx x
  eraFunBabbage = mkConsensusTx (S . S . S . S . S . Z)

  eraFunConway ∷ Tx Conway → NoIdx ConsensusTx x
  eraFunConway = mkConsensusTx (S . S . S . S . S . S . Z)

  mkGenTx n = NoIdx . Consensus.HardForkGenTx . Consensus.OneEraGenTx . n
  mkConsensusTx n = mkGenTx n . CS.mkShelleyTx . unwrapTx
