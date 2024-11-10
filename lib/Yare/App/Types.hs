module Yare.App.Types
  ( Config
  , Configᵣ
  , NetworkInfo (..)
  , StorageMode (..)
  ) where

import Yare.Prelude

import Cardano.Api.Ledger (Network, StrictMaybe)
import Cardano.Api.Shelley
  ( BabbageEraOnwards
  , LedgerEpochInfo
  , LedgerProtocolParameters
  , SystemStart
  )
import Network.Wai.Handler.Warp qualified as Warp
import Ouroboros.Network.Magic (NetworkMagic)
import Yare.Chain.Types (ChainPoint, DatabasePath, MnemonicPath)
import Yare.Node.Socket (NodeSocket)

type Configᵣ =
  '[ Warp.Port
   , NodeSocket
   , NetworkMagic
   , MnemonicPath
   , StorageMode DatabasePath
   , Tagged "syncFrom" (StrictMaybe ChainPoint)
   ]

type Config = HList Configᵣ

data NetworkInfo era = NetworkInfo
  { network ∷ Network
  , systemStart ∷ SystemStart
  , epochInfo ∷ LedgerEpochInfo
  , currentEra ∷ BabbageEraOnwards era
  , protocolParameters ∷ LedgerProtocolParameters era
  }

data StorageMode diskData = InMemory | OnDisk diskData
