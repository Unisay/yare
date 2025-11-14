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

import Yare.Prelude

import Cardano.Api (TxId, TxIn, fromMaryValue, lovelaceToValue)
import Cardano.Api qualified as CA
import Cardano.Api.Byron qualified as CAB
import Cardano.Api qualified as CAS
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
import Control.Lens (folded, to, (^.), (^..))
import Data.SOP.Strict (NS (S, Z))
import Fmt (Buildable, blockListF, build, nameF)
import Fmt.Orphans ()
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
  TxInEra Shelley = ShelleyTx ShelleyEra
  TxInEra Allegra = ShelleyTx AllegraEra
  TxInEra Mary = ShelleyTx MaryEra
  TxInEra Alonzo = AlonzoTx AlonzoEra
  TxInEra Babbage = AlonzoTx BabbageEra
  TxInEra Conway = AlonzoTx ConwayEra

-- Newtype wrapper around the TxInEra type family to allow partial application,
-- as type families cannot be partially applied.
newtype Tx (era ∷ Era) = Tx {unwrapTx ∷ TxInEra era}

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

type family TxBodyForEra (era ∷ Era) ∷ Type where
  TxBodyForEra Byron = Byron.TxAux
  TxBodyForEra Shelley = ShelleyTxBody ShelleyEra
  TxBodyForEra Allegra = AllegraTxBody AllegraEra
  TxBodyForEra Mary = MaryTxBody MaryEra
  TxBodyForEra Alonzo = AlonzoTxBody AlonzoEra
  TxBodyForEra Babbage = BabbageTxBody BabbageEra
  TxBodyForEra Conway = ConwayTxBody ConwayEra

newtype TxBody (era ∷ Era) = TxBody (TxBodyForEra era)

--------------------------------------------------------------------------------
-- Tx outputs ------------------------------------------------------------------

type family TxOutForEra (era ∷ Era) ∷ Type where
  TxOutForEra Byron = Byron.TxOut
  TxOutForEra Shelley = ShelleyTxOut ShelleyEra
  TxOutForEra Allegra = ShelleyTxOut AllegraEra
  TxOutForEra Mary = ShelleyTxOut MaryEra
  TxOutForEra Alonzo = AlonzoTxOut AlonzoEra
  TxOutForEra Babbage = BabbageTxOut BabbageEra
  TxOutForEra Conway = BabbageTxOut ConwayEra

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

instance Buildable TxViewUtxo where
  build TxViewUtxo {..} =
    nameF "TxId" (build txViewId)
      <> nameF "Inputs" (blockListF (map build txViewInputs))
      <> nameF "Outputs" (blockListF (map build txViewOutputs))

-- | Tx output view for the purpose of tracking unspent tx outputs (UTxO)
data TxOutViewUtxo = TxOutViewUtxo
  { txOutViewUtxoIndex ∷ CA.TxIx
  , txOutViewUtxoAddress ∷ LedgerAddress
  , txOutViewUtxoValue ∷ CA.Value
  }
  deriving stock (Eq, Show)

instance Buildable TxOutViewUtxo where
  build TxOutViewUtxo {..} =
    nameF "Index" (build txOutViewUtxoIndex)
      <> nameF "Address" (build txOutViewUtxoAddress)
      <> nameF "Value" (build txOutViewUtxoValue)

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
