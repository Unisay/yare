module Yare.Chain.Block
  ( StdCardanoBlock

    -- * Functors, indexed by Block
  , IxedByBlock (..)
  , hoistIxedByBlock

    -- * blocks
  , Block (..)
  , Blocks
  , StdShelleyBlocks
  ) where

import Yare.Prelude

import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import Ouroboros.Consensus.Cardano.Block (CardanoShelleyEras)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Eras
  ( AllegraEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , MaryEra
  , ShelleyEra
  , StandardCrypto
  )
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Yare.Chain.Era (Era (..))
import Ouroboros.Consensus.Cardano (CardanoBlock)

type Blocks =
  [ BlockInEra Byron
  , BlockInEra Shelley
  , BlockInEra Allegra
  , BlockInEra Mary
  , BlockInEra Alonzo
  , BlockInEra Babbage
  , BlockInEra Conway
  ]

type StdShelleyBlocks ∷ [Type]
type StdShelleyBlocks = CardanoShelleyEras StandardCrypto

type StdCardanoBlock = CardanoBlock StandardCrypto

-- | Type family that maps an era to the corresponding Block type.
type BlockInEra ∷ Era → Type
type family BlockInEra (era ∷ Era) where
  BlockInEra Byron =
    ByronBlock
  BlockInEra Shelley =
    ShelleyBlock (TPraos StandardCrypto) (ShelleyEra StandardCrypto)
  BlockInEra Allegra =
    ShelleyBlock (TPraos StandardCrypto) (AllegraEra StandardCrypto)
  BlockInEra Mary =
    ShelleyBlock (TPraos StandardCrypto) (MaryEra StandardCrypto)
  BlockInEra Alonzo =
    ShelleyBlock (TPraos StandardCrypto) (AlonzoEra StandardCrypto)
  BlockInEra Babbage =
    ShelleyBlock (Praos StandardCrypto) (BabbageEra StandardCrypto)
  BlockInEra Conway =
    ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)

-- Newtype wrapper around the type family to allow partial application,
-- as type families cannot be partially applied.
type Block ∷ Era → Type
newtype Block (era ∷ Era) = Block (BlockInEra era)

type IxedByBlock ∷ (Type → Type) → Type
data IxedByBlock (f ∷ Type → Type)
  = IxedByBlockByron (f (BlockInEra Byron))
  | IxedByBlockShelley (f (BlockInEra Shelley))
  | IxedByBlockAllegra (f (BlockInEra Allegra))
  | IxedByBlockMary (f (BlockInEra Mary))
  | IxedByBlockAlonzo (f (BlockInEra Alonzo))
  | IxedByBlockBabbage (f (BlockInEra Babbage))
  | IxedByBlockConway (f (BlockInEra Conway))

hoistIxedByBlock ∷ ∀ f g. (∀ a. f a → g a) → IxedByBlock f → IxedByBlock g
hoistIxedByBlock f = \case
  IxedByBlockByron b → IxedByBlockByron $ f b
  IxedByBlockShelley b → IxedByBlockShelley $ f b
  IxedByBlockAllegra b → IxedByBlockAllegra $ f b
  IxedByBlockMary b → IxedByBlockMary $ f b
  IxedByBlockAlonzo b → IxedByBlockAlonzo $ f b
  IxedByBlockBabbage b → IxedByBlockBabbage $ f b
  IxedByBlockConway b → IxedByBlockConway $ f b
