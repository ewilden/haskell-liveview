{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

import LiveView.Examples.Carcassonne.Types
import LiveView.Examples.Carcassonne.Reducer

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

{-
Perhaps we should start with some generators for board setups?
One idea is to write a valid-turn generator and then generate board
setups iteratively via generating valid turns.
Then the number of turns until cutoff is a natural shrinkage dimension.
Also, we should bias tile placement (e.g. toward lining up more edges)
-}

prop_flipLRUDOne_twice_id :: Property
prop_flipLRUDOne_twice_id = property $ do
  x <- forAll Gen.enumBounded
  flipLRUDOne (flipLRUDOne x) === x

prop_rotateLRUDCcw_four_id :: Property
prop_rotateLRUDCcw_four_id = property $ do
  [a, b, c, d] <- replicateM 4 (forAll Gen.alpha)
  let x = LRUD a b c d
  x === iterate rotateLRUDCcw x !! 4

prop_toFromLRUDLens_id :: Property
prop_toFromLRUDLens_id = property $ do
  x <- forAll Gen.enumBounded
  x === fromLRUDLens (toLRUDLens x)

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
