{-# LANGUAGE TemplateHaskell #-}

import Import
import           Hedgehog
import Data.Aeson
import Data.Algorithm.Diff
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LiveView.Html
import LiveView.Serving
import LiveView.Fixtures
import Streaming
import qualified Streaming.Prelude as S

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

prop_serve_onePatchPerState :: Property
prop_serve_onePatchPerState = property $ do
  let mkInt = Gen.int (Range.linear 0 100)
  inputs <- forAll $ genStateOnlyLiveViewInputs mkInt
  initR <- forAll mkInt
  let outputs :: LiveViewOutputs () Identity
      outputs = serveLiveView testIntLiveView (LiveViewInputs initR $ S.each inputs)
  length (filter (\case (OutputPatch _) -> True; _ -> False) $ 
    runIdentity $ S.toList_ (_outputStream outputs)) === length inputs

tests :: IO Bool
tests = checkParallel $$(discover)
