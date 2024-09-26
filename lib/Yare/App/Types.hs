{-# LANGUAGE TemplateHaskell #-}

module Yare.App.Types
  ( Config (..)
  , NetworkInfo (..)
  , AppState (..)
  , chainState
  , addressState
  , initialState
  ) where

import Relude

import Cardano.Api.Ledger (Network)
import Cardano.Api.Shelley
  ( LedgerEpochInfo
  , LedgerProtocolParameters
  , SystemStart
  , BabbageEraOnwards
  )
import Control.Lens.TH (makeLenses)
import Data.Tagged (Tagged)
import Network.Wai.Handler.Warp qualified as Warp
import NoThunks.Class (NoThunks)
import Ouroboros.Network.Magic (NetworkMagic)
import Path (Abs, File, Path)
import Yare.Addresses (Addresses)
import Yare.Chain.Follower (ChainState, initialChainState)
import Yare.Chain.Point (ChainPoint)
import Yare.Node.Socket (NodeSocket)

-- | An application configuration
type Config ∷ Type
data Config = Config
  { apiHttpPort ∷ Warp.Port
  , nodeSocket ∷ NodeSocket
  , networkMagic ∷ NetworkMagic
  , mnemonicFile ∷ Tagged "mnemonic" (Path Abs File)
  , syncFrom ∷ Maybe ChainPoint -- TODO: let app decide it based on the persistent state
  }

type NetworkInfo ∷ Type → Type
data NetworkInfo era = NetworkInfo
  { network ∷ Network
  , systemStart ∷ SystemStart
  , epochInfo ∷ LedgerEpochInfo
  , currentEra ∷ BabbageEraOnwards era
  , protocolParameters ∷ LedgerProtocolParameters era
  }

type AppState ∷ Type
data AppState = AppState
  { _chainState ∷ !ChainState
  , _addressState ∷ !Addresses
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

initialState ∷ Addresses → AppState
initialState _addressState =
  AppState {_chainState = initialChainState, _addressState}

$(makeLenses ''AppState)
