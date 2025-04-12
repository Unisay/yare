module Spec.Services (spec) where

import Test.Syd (it)
import Test.Syd.Def (Spec, describe)
import Yare.Prelude

spec ∷ HasCallStack ⇒ Spec
spec = describe "Services" do
  describe "Rebalancing" do
    it "is always true" True
