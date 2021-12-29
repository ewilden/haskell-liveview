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
import Data.List (sortOn, findIndex, elemIndex, foldl', sort)
import Data.Either (isRight)

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

addGenTileToBoard :: (MonadGen m) => m Tile -> Board -> m Board
addGenTileToBoard genTile' board = do
  tile <- genTile'
  let viableCandidates = possiblePlacements board tile
  when (null viableCandidates) Gen.discard
  (loc,tile) <- Gen.element viableCandidates
  pure $ board & xyToTile . at loc ?~ tile

addSomeTileToBoard :: forall m. (MonadGen m) => Board -> m Board
addSomeTileToBoard = addGenTileToBoard genTile

genLoc :: MonadGen m => m (Int, Int)
genLoc =
  sequenceT $ ((), ()) & both .~ Gen.int (Range.linearFrom 0 (-99) 99)

genBoardWithTiles :: MonadGen m => m Tile -> m Board
genBoardWithTiles genTile' =
  Gen.recursive
    Gen.choice
    [ Gen.constant (Board (HM.singleton (0,0) startingTile))
    ]
    [ Gen.subtermM genBoard (addGenTileToBoard genTile')
    ]

genBoard :: MonadGen m => m Board
genBoard = genBoardWithTiles genTile

countJusts :: [Maybe a] -> Int
countJusts = length . catMaybes

prop_computeTileBounds_areTight :: Property
prop_computeTileBounds_areTight = property $ do
  b <- forAll genBoard
  let LRUD x0 x1 y1 y0 = computeTileBounds b
      (xs, ys) = unzip $ b ^. xyToTile . to HM.keys
  assert (x0 `elem` xs)
  assert (x1 `elem` xs)
  assert (y0 `elem` ys)
  assert (y1 `elem` ys)

prop_allTilesInitiallyPlaceable :: Property
prop_allTilesInitiallyPlaceable = property $ do
  tile <- forAll genTile
  let board = Board (HM.singleton (0,0) startingTile)
      tileRotates = take 4 (iterate rotateCcw tile)
      isOccupied (loc,_) = board ^. xyToTile . at loc . to isJust
      isPlaceable x = canPlaceOnBoard x board
      isAllowed x = not (isOccupied x) && isPlaceable x
      isPlaceableSomewhere = any isAllowed $ do
        let LRUD x0 x1 y1 y0 = computeTileBounds board
        x <- [(pred x0)..(succ x1)]
        y <- [(pred y0)..(succ y1)]
        tile' <- tileRotates
        pure ((x,y), tile')
  assert isPlaceableSomewhere

genScorableSideTerrain :: (MonadGen m) => m SideTerrain
genScorableSideTerrain = intoTerrain <$> Gen.enumBounded

prop_numCompleteComponents_isMonotonic :: Property
prop_numCompleteComponents_isMonotonic = property $ do
  board <- forAll genBoard
  board' <- forAll $ addSomeTileToBoard board
  terrain <- forAll genScorableSideTerrain
  diff (length (terrainCompleteComponents terrain board))
      (<=) (length (terrainCompleteComponents terrain board'))

prop_numCompleteComponents_withOnlyStraightRoads_neverIncreases :: Property
prop_numCompleteComponents_withOnlyStraightRoads_neverIncreases = property $ do
  let tileGen = Gen.constant straightRoadTile
  board <- forAll $ genBoardWithTiles tileGen
  board' <- forAll $ addGenTileToBoard tileGen board
  terrain <- forAll genScorableSideTerrain
  length (terrainCompleteComponents terrain board)
      === length (terrainCompleteComponents terrain board')

prop_facingSides_matchTileSides :: Property
prop_facingSides_matchTileSides = property $ do
  board <- forAll genBoard
  loc <- forAll $ Gen.element (board ^. xyToTile . to HM.keys)
  let
      nbrs = tileNeighborhood loc board
      nbrSides = facingSides nbrs
      isNothingOrEqual tileSide nbrSide = maybe (pure ()) (=== tileSide) nbrSide
  sequence_ $
    isNothingOrEqual <$> (board ^?! xyToTile . ix loc . sides) <*> nbrSides

genMessages :: (MonadGen m) => GameState -> m [Message]
genMessages gs = case gs ^. gameWhoseTurn . whoseTurnPhase of
  PhaseTile -> case gs ^? gameTiles . ix 0 of
      Nothing -> pure []
      Just tile ->
        let places = possiblePlacements (gs ^. gameBoard) tile
        in
          if null places then error ("no possible places for tile " ++ show gs) else
          Gen.element $ flip fmap places $ \(loc, tile') ->
          let requiredRotates = fromJust $ elemIndex tile' (take 4 $ iterate rotateCcw tile)
          in replicate requiredRotates CurrentTileRotateLeft <> [PlaceTile loc]
  PhasePlaceMeeple loc -> do
    msg <- Gen.element $ filter (isRight . flip guardedReducer gs) $ PlaceMeeple loc Nothing :
      (validMeeplePlacements loc gs <&> (PlaceMeeple loc . Just))
    pure [msg]
  PhaseTakeAbbot -> pure [TakeAbbot Nothing]
  PhaseGameOver -> pure []

genGameState :: (MonadGen m) => m GameState
genGameState = Gen.recursive Gen.choice
  [ do
    n <- Gen.element [2..4]
    initGameState Gen.shuffle n ]
  [ Gen.subtermM genGameState (\gs -> do
    msgs <- genMessages gs
    pure $ foldl' (flip reducer) gs msgs
    )]

genGameStateSatisfying :: (MonadGen m) => (GameState -> Bool) -> m GameState
genGameStateSatisfying f = do
  let go gs =
        if f gs then pure gs
        else do
          msgs <- genMessages gs
          if null msgs then Gen.discard
          else go (foldl' (flip reducer) gs msgs)
  gs <- genGameState
  go gs


prop_belowMeepleLimits :: Property
prop_belowMeepleLimits = property $ do
  gs <- forAll genGameState
  let counts = countMeeples gs
  forM_ (HM.elems counts) (assert . isBelowMeepleLimits)

prop_genGameStateSatisfying_works :: Property
prop_genGameStateSatisfying_works = property $ do
  gs <- forAll $ genGameStateSatisfying (\gs -> gs ^. gameWhoseTurn . whoseTurnPhase == PhaseTile)
  (gs ^. gameWhoseTurn. whoseTurnPhase) === PhaseTile

prop_alwaysPlaceable :: Property
prop_alwaysPlaceable = property $ do
  gs <- forAll $ genGameStateSatisfying (\gs -> gs ^. gameWhoseTurn . whoseTurnPhase == PhaseTile)
  let isPlaceable = isJust $ do
        tile <- gs ^? gameTiles . ix 0
        guard $ not $ null (possiblePlacements (gs ^. gameBoard) tile)
  assert isPlaceable

prop_canPlaceMeepleOnGloballyUnmeepledTerrain :: Property
prop_canPlaceMeepleOnGloballyUnmeepledTerrain = property $ do
  let selector :: GameState -> Maybe ((Int, Int), [SideTerrain])
      selector gs = (case gs ^. gameWhoseTurn . whoseTurnPhase of
        PhasePlaceMeeple loc ->
          let tile = gs ^?! xyToTile . ix loc
              sideTerrains = foldr (:) [] $ tile ^. sides
              globallyUnmeepledTerrains = filter (/= Field) $ flip filter sideTerrains $ \terr ->
                flip all (gs ^. xyToTile . to HM.elems) $ \tile -> isNothing $ do
                  (mplc, _) <- tile ^. tileMeeplePlacement
                  case mplc of
                    PlaceSide lrudOne -> if tile ^. sides . toLRUDLens lrudOne == terr then Just () else Nothing
                    _ -> Nothing
          in if null globallyUnmeepledTerrains then Nothing else Just (loc, globallyUnmeepledTerrains)
        _ -> Nothing)
  gs <- forAll $ genGameStateSatisfying (isJust . selector)
  let Just (loc, relevantGloballyUnmeepledTerrains) = selector gs
  let placements = validMeeplePlacements loc gs
      tile = gs ^?! xyToTile . ix loc
  sort (mapMaybe (\case PlaceSide lrudOne -> if (tile ^. sides . toLRUDLens lrudOne) `elem` relevantGloballyUnmeepledTerrains
                                               then Just $ tile ^. sides . toLRUDLens lrudOne
                                               else Nothing
                        _ -> Nothing) placements
        ) === sort relevantGloballyUnmeepledTerrains

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO Bool
main = tests

{- 
Current failures:
recheck (Size 91) (Seed 1195665561685055342 12918760422426807891) prop_alwaysPlaceable
recheck (Size 48) (Seed 4493440989694573018 16655293125463297363) prop_canPlaceMeepleOnGloballyUnmeepledTerrain

-}