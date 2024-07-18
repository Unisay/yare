module Yare.App.Types
  ( Config (..)
  , Services (..)
  , NetworkInfo (..)
  ) where

import Relude

import Cardano.Api.Shelley
  ( InAnyShelleyBasedEra
  , LedgerEpochInfo
  , LedgerProtocolParameters
  , ShelleyBasedEra
  , SystemStart
  , TxBodyErrorAutoBalance
  )
import Data.Tagged (Tagged)
import Data.Variant (Variant)
import Network.Wai.Handler.Warp qualified as Warp
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr, StandardCrypto)
import Ouroboros.Network.Magic (NetworkMagic)
import Path (Abs, File, Path)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Node.Socket (NodeSocket)
import Yare.Utxo (Utxo)

-- | An application configuration
type Config ∷ Type
data Config = Config
  { apiHttpPort ∷ Warp.Port
  , nodeSocket ∷ NodeSocket
  , networkMagic ∷ NetworkMagic
  , mnemonicFile ∷ Tagged "mnemonic" (Path Abs File)
  , syncFrom ∷ Maybe ChainPoint -- TODO: let app decide it based on the persistent state
  , txId ∷ ByteString
  }

type NetworkInfo ∷ Type → Type
data NetworkInfo era = NetworkInfo
  { systemStart ∷ SystemStart
  , epochInfo ∷ LedgerEpochInfo
  , currentEra ∷ ShelleyBasedEra era
  , protocolParameters ∷ LedgerProtocolParameters era
  }

-- | Application services
type Services ∷ Type
data Services = Services
  { serveUtxo ∷ IO Utxo
  , serveTip ∷ IO ChainTip
  , deployScript
      ∷ IO
          ( Maybe
              ( Variant
                  [ CardanoApplyTxErr StandardCrypto
                  , InAnyShelleyBasedEra TxBodyErrorAutoBalance
                  ]
              )
          )
  }
