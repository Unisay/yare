module Main where

import Yare.Prelude

import Spec.Utxo qualified as Utxo
import Spec.Yare.App.Services.Rebalancing qualified as Rebalancing
import Test.Syd (sydTest)
import Test.Syd.Def (Spec)

main ∷ IO ()
main = sydTest spec

spec ∷ Spec
spec = do
  Utxo.spec
  Rebalancing.spec
