module Yare.Chain.Block.Reference where

import Yare.Prelude

import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Codec.Serialise.Class (Serialise)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
  ( HeaderHash
  , blockHash
  , blockNo
  , blockSlot
  , pattern BlockPoint
  )
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Point (ChainPoint)

data BlockRef = BlockRef
  { blockRefSlot ∷ !SlotNo
  , blockRefBlock ∷ !BlockNo
  , blockRefHash ∷ !(HeaderHash StdCardanoBlock)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

blockRef ∷ StdCardanoBlock → BlockRef
blockRef block =
  BlockRef
    { blockRefSlot = blockSlot block
    , blockRefBlock = blockNo block
    , blockRefHash = blockHash block
    }

blockRefPoint ∷ BlockRef → ChainPoint
blockRefPoint BlockRef {..} = BlockPoint blockRefSlot blockRefHash
