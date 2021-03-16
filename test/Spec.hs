{-# LANGUAGE TemplateHaskell #-}

import Import
import           Hedgehog
import Data.Aeson
import Data.Algorithm.Diff
import Data.Text qualified as T
import Control.Monad.Except
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

-- prop_serve_onePatchPerState :: Property
-- prop_serve_onePatchPerState = property $ do
--   let mkInt = Gen.int (Range.linear 0 100)
--   inputs <- forAll $ genStateOnlyLiveViewInputs mkInt
--   initR <- forAll mkInt
--   let outputs :: LiveViewOutputs Identity
--       outputs = serveLiveView testIntLiveView (LiveViewInputs initR $ S.each inputs)
--   length (filter (\case (OutputPatch _) -> True; _ -> False) $ 
--     runIdentity $ S.toList_ (_outputStream outputs)) === length inputs

-- prop_serve_clockCountFrom0 :: Property 
-- prop_serve_clockCountFrom0 = property $ do
--   let mkInt = Gen.int (Range.linear 0 100)
--   inputs <- forAll $ genStateOnlyLiveViewInputs mkInt
--   initR <- forAll mkInt
--   let outputs :: LiveViewOutputs Identity
--       outputs = serveLiveView testIntLiveView (LiveViewInputs initR $ S.each inputs)
--       clockInts = _unClock (snd $ _mountList outputs) : (mapMaybe (\case (OutputPatch a) -> Just $ _unClock $ snd a; _ -> Nothing) $ 
--                       runIdentity $ S.toList_ (_outputStream outputs))
--   clockInts === [0..(length clockInts - 1)]

-- prop_serveAndApply_noPathDependence :: Property
-- prop_serveAndApply_noPathDependence = property $ do
--   let mkInt = Gen.int (Range.linear 0 100)
--   inputs <- forAll $ genStateOnlyLiveViewInputs mkInt
--   inputs' <- forAll $ genStateOnlyLiveViewInputs mkInt
--   initR <- forAll mkInt
--   initR' <- forAll mkInt
--   endR <- forAll mkInt
--   let outputs :: LiveViewOutputs Identity
--       outputs = serveLiveView testIntLiveView (LiveViewInputs initR $ S.each (inputs ++ [InputState endR]))
--       outputs' :: LiveViewOutputs Identity
--       outputs' = serveLiveView testIntLiveView (LiveViewInputs initR' $ S.each (inputs' ++ [InputState endR]))
--       toFinalList :: LiveViewOutputs Identity -> PropertyT IO [T.Text]
--       toFinalList outs = toClientsidePatchlist outs & S.last_ & runExcept & evalEither >>= evalMaybe & (fmap fst)
--   out <- toFinalList outputs
--   out' <- toFinalList outputs'
--   out === out'

tests :: IO Bool
tests = checkParallel $$(discover)

main = tests
