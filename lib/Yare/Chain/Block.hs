module Yare.Chain.Block
  ( HFBlock

    -- * Functors, indexed by Block
  , IxedByBlock (..)
  , hoistIxedByBlock

    -- * blocks
  , Block (..)
  , Blocks
  , StdShelleyBlocks
  ) where

import Relude

import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import Ouroboros.Consensus.Cardano.Block (CardanoShelleyEras, HardForkBlock)
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

type Blocks =
  [ BlockForEra Byron
  , BlockForEra Shelley
  , BlockForEra Allegra
  , BlockForEra Mary
  , BlockForEra Alonzo
  , BlockForEra Babbage
  , BlockForEra Conway
  ]

type StdShelleyBlocks = CardanoShelleyEras StandardCrypto

type HFBlock = HardForkBlock Blocks

-- | Type family that maps an era to the corresponding Block type.
type family BlockForEra (era ∷ Era) where
  BlockForEra Byron =
    ByronBlock
  BlockForEra Shelley =
    ShelleyBlock (TPraos StandardCrypto) (ShelleyEra StandardCrypto)
  BlockForEra Allegra =
    ShelleyBlock (TPraos StandardCrypto) (AllegraEra StandardCrypto)
  BlockForEra Mary =
    ShelleyBlock (TPraos StandardCrypto) (MaryEra StandardCrypto)
  BlockForEra Alonzo =
    ShelleyBlock (TPraos StandardCrypto) (AlonzoEra StandardCrypto)
  BlockForEra Babbage =
    ShelleyBlock (Praos StandardCrypto) (BabbageEra StandardCrypto)
  BlockForEra Conway =
    ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)

-- Newtype wrapper around the type family to allow partial application,
-- as type families cannot be partially applied.
newtype Block (era ∷ Era) = Block (BlockForEra era)

data IxedByBlock (f ∷ Type → Type)
  = IxedByBlockByron (f (BlockForEra Byron))
  | IxedByBlockShelley (f (BlockForEra Shelley))
  | IxedByBlockAllegra (f (BlockForEra Allegra))
  | IxedByBlockMary (f (BlockForEra Mary))
  | IxedByBlockAlonzo (f (BlockForEra Alonzo))
  | IxedByBlockBabbage (f (BlockForEra Babbage))
  | IxedByBlockConway (f (BlockForEra Conway))

hoistIxedByBlock ∷ ∀ f g. (∀ a. f a → g a) → IxedByBlock f → IxedByBlock g
hoistIxedByBlock f = \case
  IxedByBlockByron b → IxedByBlockByron $ f b
  IxedByBlockShelley b → IxedByBlockShelley $ f b
  IxedByBlockAllegra b → IxedByBlockAllegra $ f b
  IxedByBlockMary b → IxedByBlockMary $ f b
  IxedByBlockAlonzo b → IxedByBlockAlonzo $ f b
  IxedByBlockBabbage b → IxedByBlockBabbage $ f b
  IxedByBlockConway b → IxedByBlockConway $ f b
