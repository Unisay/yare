module Yare.Chain.Point
  ( ChainPoint
  , parseChainPoint
  ) where

import Relude

import Cardano.Ledger.BaseTypes (WithOrigin (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BSC
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Network.Block (Point (..))
import Ouroboros.Network.Point (Block (..))
import Yare.Chain.Block (StdCardanoBlock)

type ChainPoint ∷ Type
type ChainPoint = Point StdCardanoBlock

parseChainPoint ∷ String → Either String ChainPoint
parseChainPoint src = do
  (hash, slot) ← case break (== ':') src of
    (hash, colonSlot)
      | slot ← drop 1 colonSlot
      , not (null hash) && not (null slot) →
          Right (hash, slot)
    _ → Left $ "Invalid chain point specified (hash:slot): " <> src
  blockPointSlot ←
    maybeToRight ("Invalid chain point slot: " <> slot) $
      SlotNo <$> readMaybe (toString slot)
  blockPointHash ←
    OneEraHash . toShort <$> B16.decode (BSC.pack hash)
  pure . Point . At $ Block {blockPointSlot, blockPointHash}
