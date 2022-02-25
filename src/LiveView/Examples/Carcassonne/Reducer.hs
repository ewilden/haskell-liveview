{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne.Reducer where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.NonEmpty.AdjacencyMap (fromNonEmpty)
import Algebra.Graph.ToGraph (ToGraph (toAdjacencyMap))
import Algebra.Graph.Undirected qualified as Undirected
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.Foldable
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.HashMap.Monoidal qualified as MHM
import Data.List (sortOn)
import Data.List.Extra (groupSort, genericLength)
import Data.Semigroup (Sum(..), Max(..))
import Data.String (fromString)
import Data.Text qualified as T
import Data.Tuple.Extra (swap)
import Debug.Trace
import Focus qualified
import Import
import Lib
import ListT qualified
import LiveView
import LiveView.Examples.Carcassonne.Tiles
import LiveView.Examples.Carcassonne.Types
import LiveView.Html
import LiveView.Serving
import LiveView.Serving.Servant
import Lucid
import Lucid.Base (commuteHtmlT)
import Network.Wai.Handler.Warp qualified as Warp
import Servant hiding (Stream)
import StmContainers.Map qualified as StmMap
import Text.Read hiding (get)
import qualified Data.Bifunctor
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

initBoard :: Board
initBoard = Board (HM.singleton (0, 0) startingTile)

initGameState :: (Monad m) => (forall a. [a] -> m [a]) -> NumPlayers -> m GameState
initGameState shuffle numPlayers = do
  initialTiles <- shuffle unshuffledTiles
  pure $
    GameState
      { _gameBoard = Board (HM.singleton (0, 0) startingTile),
        _gameTiles = initialTiles,
        _gameNumPlayers = numPlayers,
        _gameWhoseTurn = WhoseTurn 0 PhaseTile,
        _gameScores = case numPlayers of NumPlayers n ->
                                           HM.fromList $ take (fromIntegral n) $ zip (PlayerIndex <$> [0..]) (repeat 0),
        _gameMostRecentError = ""
      }

initGameRoomContext :: (MonadRandom m) => m GameRoomContext
initGameRoomContext = do
  gameState <- initGameState shuffle (NumPlayers 2)
  pure $ GameRoomContext
    { _grGameState = gameState,
      _userId2Player = mempty
    }

terrainEdges ::
  (HasBoard b) =>
  SideTerrain ->
  (Int, Int) ->
  b ->
  [(TerrainGraphKey, TerrainGraphKey)]
terrainEdges terrain loc gs =
  let tile = gs ^?! xyToTile . ix loc
      nbrs = tileNeighborhood loc gs
      nbrSides = facingSides nbrs
      toMayEdge loc' maySide lrudOne amITerm = case maySide of
        Just nbrTerrain ->
          if nbrTerrain /= terrain then Nothing
          else let thisEdge = tile ^. sides . toLRUDLens lrudOne in
            if nbrTerrain /= thisEdge
            then error $
                 "impossible: different terrains neighboring   "
                 ++ show (gs ^. board)
                --  ++ show loc'
                --  ++ show maySide
                --  ++ show lrudOne
                --  ++ show amITerm
                --  ++ show nbrTerrain
                --  ++ show terrain
                --  ++ show thisEdge
            else
              Just
                ( TerrainGraphKey loc lrudOne,
                  TerrainGraphKey loc' (flipLRUDOne lrudOne)
                )
        Nothing ->
          Just
            ( TerrainGraphKey loc lrudOne,
              TerrainEmptyKey
            )
      edgesToNbrs =
        catMaybes $
          ( toMayEdge
              <$> (lrudNeighbors <*> pure loc)
              <*> nbrSides
              <*> lrudOnes
              <*> (snd <$> sidesWithIsTerminus tile)
          )
            ^.. traverse
      hasInternalEdges =
        (> 0) $
          length $
            filter (not . snd) $ filter (\(t, _) -> t == terrain) $
              sidesWithIsTerminus tile ^.. traverse
      applicableSides =
        filter
          (\s -> terrain == tile ^. sides . toLRUDLens s)
          (lrudOnes ^.. traverse)
      internalEdges =
        if hasInternalEdges
          then do
            s <- applicableSides
            s' <- applicableSides
            guard (s /= s')
            [(TerrainGraphKey loc s, TerrainGraphKey loc s')]
          else []
   in edgesToNbrs <> internalEdges

buildTerrainGraph :: (HasBoard a) => SideTerrain -> a -> Undirected.Graph TerrainGraphKey
buildTerrainGraph terrain gs = HM.foldlWithKey' folder Undirected.empty (gs ^. xyToTile)
  where
    folder graph loc tile = Undirected.overlay graph (graph' loc)
    graph' loc =
      let edgesToAdd = terrainEdges terrain loc gs
       in Undirected.edges edgesToAdd

terrainComponents :: (HasBoard a) => SideTerrain -> a -> [HS.HashSet TerrainGraphKey]
terrainComponents terrain gs =
  let terrainAdjMap =
        toAdjacencyMap
          (Undirected.fromUndirected $ buildTerrainGraph terrain gs)
      comps = vertexList $ scc terrainAdjMap
   in comps <&> fromNonEmpty <&> vertexList <&> HS.fromList

terrainComponentsIgnoringEmptyKey :: (HasBoard a) => SideTerrain -> a
  -> [HS.HashSet TerrainGraphKey]
terrainComponentsIgnoringEmptyKey terrain gs =
  let terrainAdjMap = removeVertex TerrainEmptyKey $
        toAdjacencyMap
          (Undirected.fromUndirected $ buildTerrainGraph terrain gs)
      comps = vertexList $ scc terrainAdjMap
   in comps <&> fromNonEmpty <&> vertexList <&> HS.fromList



terrainCompleteComponents :: (HasBoard a) => SideTerrain -> a -> [[TerrainGraphKey]]
terrainCompleteComponents terrain gs = terrainComponents terrain gs
  & filter (not . HS.member TerrainEmptyKey)
  <&> HS.toList

toMeepleCount :: MeeplePlacement -> MeepleCounts
toMeepleCount PlaceAbbot = mempty { _abbots = 1 }
toMeepleCount _ = mempty { _meeples = 1 }

countMeeples :: (HasBoard a) => a -> HashMap PlayerIndex MeepleCounts
countMeeples board = MHM.getMonoidalHashMap $ mconcat $ do
  tile <- board ^. xyToTile . to HM.elems
  (meeplePlacement, playerIndex) <- tile ^.. tileMeeplePlacement . _Just
  pure $ MHM.singleton playerIndex $ toMeepleCount meeplePlacement

meepleLimits :: MeepleCounts
meepleLimits = MeepleCounts {
  _meeples = 7,
  _abbots = 1
                            }

isBelowMeepleLimits :: MeepleCounts -> Bool
isBelowMeepleLimits (MeepleCounts m a) =
  m <= meepleLimits^.meeples && a <= meepleLimits^.abbots


validMeeplePlacements :: (HasBoard a) => (Int, Int) -> a -> [MeeplePlacement]
validMeeplePlacements loc gs =
  let sidePlacements = do
        tile <- gs ^.. board . xyToTile . ix loc
        (lrudOne, sideTerrain) <- toList $ collectLRUDOnes $ tile^.sides
        case sideTerrain of
          Field -> []
          _ -> do
            let tccSets = terrainComponentsIgnoringEmptyKey sideTerrain gs
            myTcc <- filter (HS.member (TerrainGraphKey loc lrudOne)) tccSets
            let hasMeeple :: TerrainGraphKey -> Bool
                hasMeeple = \case
                  TerrainGraphKey loc' lrudOne' -> Just True == (do
                    tile' <- gs ^? board . xyToTile . ix loc'
                    (mplace, _)  <- tile' ^. tileMeeplePlacement
                    case mplace of
                      PlaceMonastery -> pure False
                      PlaceAbbot -> pure False
                      PlaceSide lrudOne' -> pure $ tile' ^. sides . toLRUDLens lrudOne' == sideTerrain)
                  TerrainEmptyKey -> False
            [PlaceSide lrudOne | not (any hasMeeple myTcc)]
      centerPlacements = do
        tile <- gs ^.. board . xyToTile . ix loc
        case tile^.middle of
          MMonastery -> [PlaceMonastery, PlaceAbbot]
          _ -> []
  in centerPlacements ++ sidePlacements

-- For a given TerrainGraphKey returns the player owning a meeple on that
-- terrain/side, if any.
-- Note that this only handles non-Monastery terrains.
collectPlacedMeeple' :: TerrainGraphKey -> GameState -> (Maybe PlayerIndex, GameState)
collectPlacedMeeple' TerrainEmptyKey gs = (Nothing, gs)
collectPlacedMeeple' (TerrainGraphKey loc side) gs =
  maybe (Nothing, gs) (Data.Bifunctor.first Just) $ do
   tile <- gs ^? xyToTile . ix loc
   (placement, playerInd) <- tile ^. tileMeeplePlacement
   case placement of
     PlaceSide side'
       | side == side' -> Just (playerInd,
                                gs & xyToTile . ix loc . tileMeeplePlacement .~ Nothing)
       | otherwise -> Nothing
     PlaceMonastery -> Nothing
     PlaceAbbot -> Nothing

collectPlacedMeeple :: TerrainGraphKey -> State GameState (Maybe PlayerIndex)
collectPlacedMeeple key = state $ collectPlacedMeeple' key

data ScorableTerrain = ScoreCity | ScoreRoad deriving (Enum, Bounded)

intoTerrain :: ScorableTerrain -> SideTerrain
intoTerrain ScoreCity = City
intoTerrain ScoreRoad = Road

data MonasteryCollectionMode = CompleteMeeple | CompleteAbbot | IncompleteAbbot deriving Eq

tryCollectMonastery :: MonasteryCollectionMode -> (Int, Int) -> GameState -> Either String GameState
tryCollectMonastery mcm loc gs = do
  tile <- maybe (Left $ "No tile at " ++ show loc) Right $ gs ^? gameBoard . xyToTile . ix loc
  let continue playerIndex = (do
        let nineLocs = do
              dx <- [-1, 0, 1]
              dy <- [-1, 0, 1]
              let (x,y) = loc
                  loc' = (x + dx, y + dy)
              pure loc'
            numFilled = fromIntegral $ length $ catMaybes $ nineLocs <&> \loc' -> gs ^? gameBoard . xyToTile . ix loc'
            countNeeded = case mcm of
              CompleteMeeple -> 9
              CompleteAbbot -> 9
              IncompleteAbbot -> 0
        if numFilled >= countNeeded then pure $
          gs & gameBoard . xyToTile . ix loc . tileMeeplePlacement .~ Nothing
            & (gameScores . at playerIndex) %~ Just . maybe (Score numFilled) (+ Score numFilled)
          else Left "Insufficiently filled")
  case (tile ^. tileMeeplePlacement, mcm) of
    (Just (PlaceMonastery, playerIndex), CompleteMeeple) -> continue playerIndex
    (Just (PlaceAbbot, playerIndex), CompleteAbbot) -> continue playerIndex
    (Just (PlaceAbbot, playerIndex), IncompleteAbbot) -> continue playerIndex
    _ -> Left "Not the right abbot / monastery placement"

collectAndScoreMeeples :: GameState -> GameState
collectAndScoreMeeples = execState $ do
  forM_ [minBound..maxBound] $ \terrain -> do
    doneComps <- gets $ terrainCompleteComponents $ intoTerrain terrain
    forM_ doneComps $ \keyList -> do
      mayPlayerInds <- mapM collectPlacedMeeple keyList
      let playerInds = catMaybes mayPlayerInds
          playerIndsAndCounts = MHM.toList $ foldMap (\i -> MHM.singleton i (Sum 1 :: Sum Int)) playerInds
          winners = maybe [] snd $ (^? ix 0) $ groupSort $ (\(i, Sum c) -> (Max c, i)) <$> playerIndsAndCounts
          multiplier = case terrain of
            ScoreCity -> 2
            ScoreRoad -> 1
      forM_ winners $ \playerInd ->
        gameScores . ix playerInd += Score (multiplier * genericLength keyList)
  gs <- get
  let allTiles = gs ^. gameBoard . xyToTile . to HM.toList
  forM_ allTiles $ \(loc, tile) -> forM_ [CompleteAbbot, CompleteMeeple] $ \mcm ->
    modify (\s -> case tryCollectMonastery mcm loc s of
      Left _ -> s
      Right s' -> s')

openSpots :: Board -> [(Int, Int)]
openSpots board = sortOn (\(x,y) -> x * x + y * y) $ do
  let LRUD x0 x1 y1 y0 = computeTileBounds board
  x <- [(pred x0)..(succ x1)]
  y <- [(pred y0)..(succ y1)]
  let loc = (x,y)
  guard $ isNothing $ board ^. xyToTile . at loc
  pure loc

possiblePlacements :: Board -> Tile -> [((Int, Int), Tile)]
possiblePlacements board tile =
  let locs = openSpots board
      nonRotatedCandidates = zip locs (repeat tile)
      isPlaceable x = canPlaceOnBoard x board
      isOccupied (loc,_) = board ^. xyToTile . at loc . to isJust
      isAllowed x = not (isOccupied x) && isPlaceable x
      rotatedCandidates = do
        (l,t) <- nonRotatedCandidates
        (l,) <$> [t, rotateCcw t, rotateCcw (rotateCcw t), rotateCw t]
  in filter isAllowed rotatedCandidates

guardedReducer ::  Message -> GameState -> Either String GameState
guardedReducer message gs = case (gs ^. gameWhoseTurn . whoseTurnPhase, message) of
  (PhaseTile, CurrentTileRotateLeft) -> pure $ (gameTiles . ix 0 %~ rotateCcw) gs
  (PhaseTile, CurrentTileRotateRight) -> pure $ (gameTiles . ix 0 %~ rotateCw) gs
  (PhaseTile, PlaceTile loc) -> do
    currTile <- maybe (Left "Out of tiles") Right $ gs ^? gameTiles . ix 0
    unless (canPlaceOnBoard (loc, currTile) gs) $ throwError "Can't place"
    pure $ gs & gameTiles %~ drop 1
          & gameBoard . xyToTile . at loc ?~ currTile
          -- TODO: check if out of meeples
          & gameWhoseTurn . whoseTurnPhase .~ PhasePlaceMeeple loc
  (PhaseTile, _) -> Left "Message n/a for PhaseTile"
  (PhasePlaceMeeple loc, PlaceMeeple loc' mayPlace) -> do
    let currPlayer = gs ^. gameWhoseTurn . whoseTurnPlayer
    when (loc /= loc') $ throwError "Mismatched locations for PlaceMeeple"
    let continue gs' = pure $ gs' & collectAndScoreMeeples
                           & gameWhoseTurn . whoseTurnPhase .~ PhaseTakeAbbot
    case mayPlace of
      Nothing -> continue gs
      Just placement -> do
        let validPlacements = validMeeplePlacements loc gs
        unless (placement `elem` validPlacements)
          $ Left "Invalid meeple placement"
        let playerIndex = gs ^. gameWhoseTurn . whoseTurnPlayer
            afterCounts = toMeepleCount placement <> (countMeeples gs ^. ix playerIndex)
        unless (isBelowMeepleLimits afterCounts) $ Left "Above meeple limits"
        advanceTurn <$> continue (gs
          & gameBoard . xyToTile . ix loc
            . tileMeeplePlacement
            ?~ (placement, currPlayer))
  (PhasePlaceMeeple _, _) -> Left "Message n/a for PhasePlaceMeeple"
  (PhaseTakeAbbot, TakeAbbot mayLoc) -> case mayLoc of
    Nothing -> pure $ advanceTurn gs
    (Just loc) -> do
      let mayPlace = gs ^? board . xyToTile . ix loc . tileMeeplePlacement . _Just
          currPlayer = gs ^. gameWhoseTurn . whoseTurnPlayer
      unless (mayPlace == Just (PlaceAbbot, currPlayer))
        $ Left "That player doesn't have an Abbot there"
      advanceTurn <$> tryCollectMonastery IncompleteAbbot loc gs
  (PhaseTakeAbbot, _) -> Left "Message n/a for PhaseTakeAbbot"
  (PhaseGameOver, _) -> Left "Message n/a for PhaseGameOver"
  where
    (NumPlayers numPlayers) = gs ^. gameNumPlayers
    advanceTurn gs' =
      let gs'' = gs' & gameWhoseTurn . whoseTurnPhase .~ PhaseTile
                  & gameWhoseTurn . whoseTurnPlayer . unPlayerIndex %~ (`mod` numPlayers) . (+1)
          step s = case s ^. gameTiles of
            [] -> s & gameWhoseTurn . whoseTurnPhase .~ PhaseGameOver
            (x:_) -> if null (possiblePlacements (gs ^. gameBoard) x)
                        then step (s & gameTiles %~ drop 1)
                        else s
      in step gs''

reducer :: Message -> GameState -> GameState
reducer msg gs = case guardedReducer msg gs of
  Right gs' -> gs'
  Left err -> gs { _gameMostRecentError = fromString err }
