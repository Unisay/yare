{-# OPTIONS_GHC -Wno-orphans #-}

module NoThunks.Class.Orphans () where

import Yare.Prelude

import Cardano.Address (Address)
import Cardano.Address.Derivation (Depth (PaymentK), XPrv)
import Cardano.Address.Style.Shelley (Shelley)
import Cardano.Api qualified as CApi
import Cardano.Crypto.Wallet qualified as Crypto
import Data.Strict.Tuple (Pair ((:!:)))
import Data.Typeable (typeRep)
import NoThunks.Class (NoThunks (..), allNoThunks)

instance NoThunks Crypto.XPrv where
  wNoThunks _ctx _xprv = pure Nothing
  showTypeOf _proxy = "XPrv"

instance NoThunks Address where
  wNoThunks _ctx _address = pure Nothing
  showTypeOf _proxy = "Address"

instance NoThunks (Shelley PaymentK XPrv) where
  wNoThunks _ctx _shelleyPaymentKXPrv = pure Nothing
  showTypeOf _proxy = "Shelley PaymentK XPrv"

instance (NoThunks a, NoThunks b) ⇒ NoThunks (Pair a b) where
  wNoThunks ctx (a :!: b) = allNoThunks [noThunks ctx a, noThunks ctx b]
  showTypeOf _proxy = "Pair"

--------------------------------------------------------------------------------
-- Tagged ----------------------------------------------------------------------

instance (Typeable s, NoThunks a) ⇒ NoThunks (Tagged s a) where
  wNoThunks ctx (Tagged a) = noThunks ctx a
  showTypeOf _proxy =
    "Tagged " <> show (typeRep (Proxy @s)) <> " " <> showTypeOf (Proxy @a)

--------------------------------------------------------------------------------
-- HList -----------------------------------------------------------------------

instance NoThunks (HList '[]) where
  wNoThunks _ctx HNil = pure Nothing
  showTypeOf _proxy = "HNil"

instance (NoThunks x, NoThunks (HList xs)) ⇒ NoThunks (HList (x ': xs)) where
  wNoThunks ctx (HCons x xs) = allNoThunks [noThunks ctx x, noThunks ctx xs]
  showTypeOf _proxy = showTypeOf (Proxy @x)

--------------------------------------------------------------------------------
-- Cardano.Api -----------------------------------------------------------------

instance NoThunks CApi.ScriptHash where
  wNoThunks ctx (CApi.ScriptHash scriptHash) =
    noThunks ("ScriptHash" : ctx) scriptHash
  showTypeOf _proxy = "ScriptHash"

instance NoThunks CApi.TxIn where
  wNoThunks _ctx _txIn = pure Nothing
  showTypeOf _proxy = "TxIn"

instance NoThunks CApi.TxId where
  wNoThunks ctx (CApi.TxId txId) = noThunks ("TxId" : ctx) txId
  showTypeOf _proxy = "TxId"

instance NoThunks CApi.Value where
  wNoThunks _ctx _value = pure Nothing
  showTypeOf _proxy = "Value"
