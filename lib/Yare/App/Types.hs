module Yare.App.Types
  ( Config
  , Configᵣ
  , NetworkInfo (..)
  , StorageMode (..)
  , Finality (..)
  ) where

import Yare.Prelude

import Cardano.Api.Ledger (Network, StrictMaybe)
import Cardano.Api.Shelley
  ( ConwayEraOnwards
  , LedgerEpochInfo
  , LedgerProtocolParameters
  , SystemStart
  )
import Network.Wai.Handler.Warp qualified as Warp
import NoThunks.Class.Extended (NoThunks)
import Ouroboros.Network.Magic (NetworkMagic)
import Yare.Chain.Types (ChainPoint, DatabasePath, MnemonicPath)
import Yare.Node.Socket (NodeSocket)

type Configᵣ =
  '[ Warp.Port
   , NodeSocket
   , NetworkMagic
   , MnemonicPath
   , DatabasePath
   , Tagged "syncFrom" (StrictMaybe ChainPoint)
   ]

type Config = HList Configᵣ

data NetworkInfo era = NetworkInfo
  { network ∷ Network
  , systemStart ∷ SystemStart
  , epochInfo ∷ LedgerEpochInfo
  , currentEra ∷ ConwayEraOnwards era
  , protocolParameters ∷ LedgerProtocolParameters era
  }

data StorageMode = Volatile | Durable
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (NoThunks, NFData)

data Finality = Final | NotFinal
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (NoThunks, NFData)
