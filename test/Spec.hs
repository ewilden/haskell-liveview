{-# LANGUAGE TemplateHaskell #-}

import Import
import           Hedgehog
import Data.Aeson
import Data.Algorithm.Diff
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import LiveView.Html

prop_diffThenPatchIsIdentity :: Property
prop_diffThenPatchIsIdentity = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  xs' <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  applyPatch xs (toPatch $ getDiff xs xs') === xs'

prop_toFromJSON :: Property
prop_toFromJSON = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  xs' <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  let patch = toPatch $ getDiff xs xs'
  patch === fromJust (decode (encode patch))

tests :: IO Bool
tests = checkParallel $$(discover)
