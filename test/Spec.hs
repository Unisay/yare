module Main where

import Test.Syd (sydTest)

import Spec.Services qualified as Services
import Spec.Utxo qualified as Utxo
import Test.Syd.Def (Spec)
import Yare.Prelude

main ∷ IO ()
main = sydTest spec

spec ∷ HasCallStack ⇒ Spec
spec = do
  Utxo.spec
  Services.spec
