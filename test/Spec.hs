{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Import
import           Hedgehog
import Data.Aeson
import Data.Algorithm.Diff
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Tuple.Sequence
import Control.Monad.Except
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LiveView.Html
import LiveView.Serving
import LiveView.Fixtures
import Streaming
import qualified Streaming.Prelude as S

import LiveView.Examples.Carcassonne.Tiles
import LiveView.Examples.Carcassonne.Types
import LiveView.Examples.Carcassonne.Reducer
import Data.Tuple (swap)
import Data.Functor ((<&>))
import Control.Lens
import Data.List (sortOn)

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
  x :: LRUDOne <- forAll Gen.enumBounded
  x === fromLRUDLens (toLRUDLens x)

prop_rotateLRUDOneCcw_four_id :: Property
prop_rotateLRUDOneCcw_four_id = property $ do
  x <- forAll Gen.enumBounded
  x === iterate rotateLRUDOneCcw x !! 4

genTile :: MonadGen m => m Tile
genTile = do
  numRotates <- Gen.element [0..4]
  fmap (\x -> iterate rotateCcw x !! numRotates) (Gen.element (fst <$> tileSpecs))
  -- ((startingTile, 1) : tileSpecs)
  --   <&> swap <&>
  --   <&> fmap pure & Gen.frequency

genLRUD :: MonadGen m => m a -> m (LRUD a)
genLRUD g = do
  (a, b, c, d) <- sequenceT (g, g, g, g)
  pure $ LRUD a b c d

prop_rotateCcw_rotateCw_inv :: Property
prop_rotateCcw_rotateCw_inv = property $ do
  x <- forAll genTile
  tripping x rotateCcw (Identity . rotateCw)

addToBoardSomewhere :: Tile -> [(Int, Int)] -> Board -> ([(Int, Int)], Board)
addToBoardSomewhere tile [] board = error "out of candidates"
addToBoardSomewhere tile (loc:candidates) board =
  let isOccupied = board ^. xyToTile . at loc . to isJust
      isPlaceable = canPlaceOnBoard (loc,tile) board
  in  if not isOccupied && isPlaceable
        then (candidates, board & xyToTile . at loc ?~ tile)
        else
          let (recurCandidates, recurBoard) = addToBoardSomewhere tile candidates board
          in (loc:recurCandidates, recurBoard)

openSpots :: Board -> [(Int, Int)]
openSpots board = sortOn (\(x,y) -> x * x + y * y) $ do
  x <- [-99..99]
  y <- [-99..99]
  let loc = (x,y)
  guard $ isNothing $ board ^. xyToTile . at loc
  pure loc


addSomeTileToBoard :: forall m. (MonadGen m) => Board -> m Board
addSomeTileToBoard board = do
  let isOccupied (loc,_) = board ^. xyToTile . at loc . to isJust
      isPlaceable x = canPlaceOnBoard x board
      isAllowed x = not (isOccupied x) && isPlaceable x
  tile <- genTile
  let locs = openSpots board
      nonRotatedCandidates = zip locs (repeat tile)
      rotatedCandidates = do
        (l,t) <- nonRotatedCandidates
        (l,) <$> [t, rotateCcw t, rotateCcw (rotateCcw t), rotateCw t]
      viableCandidates = filter isAllowed rotatedCandidates
  when (null viableCandidates) Gen.discard
  (loc,tile) <- Gen.element viableCandidates
  pure $ board & xyToTile . at loc ?~ tile

-- addSomeTileToBoard :: forall m. (MonadGen m) => Board -> m Board
-- addSomeTileToBoard board = do
--   let go :: m (Maybe Board)
--       go = do
--         loc <- genLoc
--         tile <- genTile
--         let isOccupied = board ^. xyToTile . at loc . to isJust
--             isPlaceable = canPlaceOnBoard (loc,tile) board
--         if not isOccupied && isPlaceable
--           then pure $ Just $ board & xyToTile . at loc ?~ tile
--           else pure $ Nothing
--       recGo = do
--         mayBoard <- go
--         maybe recGo pure mayBoard
--   recGo

genLoc :: MonadGen m => m (Int, Int)
genLoc =
  sequenceT $ ((), ()) & both .~ Gen.int (Range.linearFrom 0 (-99) 99)

genBoard :: MonadGen m => m Board
genBoard =
  Gen.recursive
    Gen.choice
    [ Gen.constant (Board (HM.singleton (0,0) startingTile))
    ]
    [ Gen.subtermM genBoard addSomeTileToBoard
    ]

countJusts :: [Maybe a] -> Int
countJusts = length . catMaybes

prop_computeTileBounds_hasTilesAtTwoCorners :: Property
prop_computeTileBounds_hasTilesAtTwoCorners = property $ do
  b <- forAll (Gen.filter (\b' -> b' ^. xyToTile . to HM.null . to not) genBoard)
  let LRUD x0 x1 y1 y0 = computeTileBounds b
      (xs, ys) = unzip $ b ^. xyToTile . to HM.keys
      locs = do
        x <- [x0, x1]
        y <- [y0, y1]
        pure (x,y)
      cornerCount = countJusts ((\loc -> b ^. xyToTile . at loc) <$> locs)
  assert (x0 `elem` xs)
  assert (x1 `elem` xs)
  assert (y0 `elem` ys)
  assert (y1 `elem` ys)


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
