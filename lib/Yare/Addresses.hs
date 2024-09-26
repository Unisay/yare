module Yare.Addresses
  ( Addresses
  , externalAddresses
  , deriveFromMnemonic
  , isOwnAddress
  , useForChange
  , useForFees
  , useForCollateral
  , useForScript
  , AddressConversionError (..)
  , AddressDerivationError (..)
  , NetworkMagicNoTagError (..)
  , networkMagicToLedgerNetwork
  , networkMagicToAddressesTag
  ) where

import Relude

import Cardano.Address (Address, NetworkTag)
import Cardano.Address.Style.Shelley (shelleyMainnet, shelleyTestnet)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Mnemonic (MkMnemonicError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Oops (CouldBe, CouldBeAnyOf)
import Control.Monad.Oops qualified as Oops
import Data.List.NonEmpty ((!!))
import Data.List.NonEmpty qualified as NE
import Data.Tagged (Tagged, untag)
import NoThunks.Class (NoThunks)
import Ouroboros.Network.Magic (NetworkMagic, unNetworkMagic)
import Path (Abs, File, Path)
import Yare.Address (AddressWithKey (..), externalPaymentAdressesKeys)
import Yare.Chain.Types (LedgerAddress)
import Yare.Mnemonic (mnemonicFromFile)

type Addresses ∷ Type
newtype Addresses = Addresses
  { externalAddresses ∷ NonEmpty AddressWithKey
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

deriveFromMnemonic
  ∷ ( MonadError (Oops.Variant e) m
    , e
        `CouldBeAnyOf` [ NetworkMagicNoTagError
                       , MkMnemonicError 8
                       , AddressConversionError
                       , AddressDerivationError
                       ]
    , MonadIO m
    )
  ⇒ NetworkMagic
  → Tagged "mnemonic" (Path Abs File)
  → m Addresses
deriveFromMnemonic networkMagic mnemonicFile = do
  nt ← networkMagicToAddressesTag networkMagic
  mnemonic ← mnemonicFromFile (untag mnemonicFile)
  externalLedgerAddrs ∷ NonEmpty AddressWithKey ←
    externalPaymentAdressesKeys nt mnemonic
      & take 20
      -- At least 2 addresses are needed:
      -- 1 for change, fees and collateral and 1 for the ref script.
      & NE.nonEmpty
      & Oops.hoistMaybe NoAddressesDerived
  pure Addresses {externalAddresses = force externalLedgerAddrs}

isOwnAddress ∷ Addresses → LedgerAddress → Bool
isOwnAddress Addresses {externalAddresses} address =
  address `elem` fmap ledgerAddress externalAddresses

useForChange ∷ Addresses → (Addresses, LedgerAddress)
useForChange a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for change in order to KISS.
  (a, ledgerAddress (NE.head externalAddresses))

useForFees ∷ Addresses → (Addresses, LedgerAddress)
useForFees a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for fees in order to KISS.
  (a, ledgerAddress (NE.head externalAddresses))

useForCollateral ∷ Addresses → (Addresses, LedgerAddress)
useForCollateral a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for collateral in order to KISS.
  (a, ledgerAddress (NE.last externalAddresses))

useForScript ∷ Addresses → (Addresses, LedgerAddress)
useForScript a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for script in order to KISS.
  (a, ledgerAddress (externalAddresses !! 1))

networkMagicToAddressesTag
  ∷ (MonadError (Oops.Variant e) m, e `CouldBe` NetworkMagicNoTagError)
  ⇒ NetworkMagic
  → m NetworkTag
networkMagicToAddressesTag =
  unNetworkMagic >>> \case
    764824073 → pure shelleyMainnet -- https://bityl.co/LitF
    1 → pure shelleyTestnet
    2 → pure shelleyTestnet
    n → Oops.throw (NetworkMagicNoTag n)

networkMagicToLedgerNetwork
  ∷ (MonadError (Oops.Variant e) m, e `CouldBe` NetworkMagicNoTagError)
  ⇒ NetworkMagic
  → m Ledger.Network
networkMagicToLedgerNetwork =
  unNetworkMagic >>> \case
    764824073 → pure Ledger.Mainnet
    1 → pure Ledger.Testnet
    2 → pure Ledger.Testnet
    n → Oops.throw (NetworkMagicNoTag n)

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

type NetworkMagicNoTagError ∷ Type
newtype NetworkMagicNoTagError = NetworkMagicNoTag Word32
  deriving stock (Eq, Show)

type AddressConversionError ∷ Type
newtype AddressConversionError = AddressConversionError Address
  deriving stock (Eq, Show)

type AddressDerivationError ∷ Type
data AddressDerivationError = NoAddressesDerived
  deriving stock (Eq, Show)
