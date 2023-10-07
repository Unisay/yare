module Yare.Types
  ( Block

    -- * Standard blocks
  , StdBlock
  , StdBlocks
  , liftStdBlockByron
  , liftStdBlockShelley
  , liftStdBlockAllegra
  , liftStdBlockMary
  , liftStdBlockAlonzo
  , liftStdBlockBabbage
  , liftStdBlockConway
  --
  , QueryResult
  , StdShelleyBlocks
  , Era (..)
  ) where

import Relude

import Generics.SOP (NS (..))
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , CardanoEras
  , CardanoQueryResult
  , CardanoShelleyEras
  , StandardAllegra
  , StandardAlonzo
  , StandardBabbage
  , StandardConway
  , StandardCrypto
  , StandardMary
  , StandardShelley
  )
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

type QueryResult = CardanoQueryResult StandardCrypto

data Era = Byron | Shelley | Allegra | Mary | Alonzo | Babbage | Conway

--------------------------------------------------------------------------------
-- Block type aliases ----------------------------------------------------------

type Block = CardanoBlock StandardCrypto

type StdBlocks = CardanoEras StandardCrypto

type StdShelleyBlocks = CardanoShelleyEras StandardCrypto

type family StdBlock (era ∷ Era) where
  StdBlock Byron = ByronBlock
  StdBlock Shelley = ShelleyBlock (TPraos StandardCrypto) StandardShelley
  StdBlock Allegra = ShelleyBlock (TPraos StandardCrypto) StandardAllegra
  StdBlock Mary = ShelleyBlock (TPraos StandardCrypto) StandardMary
  StdBlock Alonzo = ShelleyBlock (TPraos StandardCrypto) StandardAlonzo
  StdBlock Babbage = ShelleyBlock (Praos StandardCrypto) StandardBabbage
  StdBlock Conway = ShelleyBlock (Praos StandardCrypto) StandardConway

--------------------------------------------------------------------------------
-- Lift StdBlocks --------------------------------------------------------------

liftStdBlockByron ∷ f ByronBlock → NS f StdBlocks
liftStdBlockByron = Z

liftStdBlockShelley ∷ f (StdBlock Shelley) → NS f StdBlocks
liftStdBlockShelley = S . Z

liftStdBlockAllegra ∷ f (StdBlock Allegra) → NS f StdBlocks
liftStdBlockAllegra = S . S . Z

liftStdBlockMary ∷ f (StdBlock Mary) → NS f StdBlocks
liftStdBlockMary = S . S . S . Z

liftStdBlockAlonzo ∷ f (StdBlock Alonzo) → NS f StdBlocks
liftStdBlockAlonzo = S . S . S . S . Z

liftStdBlockBabbage ∷ f (StdBlock Babbage) → NS f StdBlocks
liftStdBlockBabbage = S . S . S . S . S . Z

liftStdBlockConway ∷ f (StdBlock Conway) → NS f StdBlocks
liftStdBlockConway = S . S . S . S . S . S . Z
