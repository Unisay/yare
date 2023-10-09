module Yare.Types
  ( Block
  , IxedByBlock (..)
  , ChainPoint
  , ChainTip

    -- * Standard blocks
  , StdBlock
  , StdBlocks
  --
  , QueryResult
  , StdShelleyBlocks
  , Era (..)
  ) where

-- These 3 imports are required as they bring instances into scope.
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

import Data.Kind (Type)
import Ouroboros.Consensus.Block (Point)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import Ouroboros.Consensus.Cardano.Block
  ( CardanoEras
  , CardanoQueryResult
  , CardanoShelleyEras
  , HardForkBlock
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
import Ouroboros.Network.Block (Tip)

type QueryResult = CardanoQueryResult StandardCrypto

data Era = Byron | Shelley | Allegra | Mary | Alonzo | Babbage | Conway

--------------------------------------------------------------------------------
-- Block type aliases ----------------------------------------------------------

type Block = HardForkBlock (CardanoEras StandardCrypto)

type ChainPoint = Point Block

type ChainTip = Tip Block

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

data IxedByBlock (f ∷ Type → Type)
  = IxedByBlockByron (f (StdBlock Byron))
  | IxedByBlockShelley (f (StdBlock Shelley))
  | IxedByBlockAllegra (f (StdBlock Allegra))
  | IxedByBlockMary (f (StdBlock Mary))
  | IxedByBlockAlonzo (f (StdBlock Alonzo))
  | IxedByBlockBabbage (f (StdBlock Babbage))
  | IxedByBlockConway (f (StdBlock Conway))
