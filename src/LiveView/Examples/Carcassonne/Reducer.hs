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
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.HashMap.Monoidal qualified as MHM
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

initGameState :: (MonadIO m) => NumPlayers -> m GameState
initGameState numPlayers = do
  initialTiles <- liftIO $ shuffle unshuffledTiles
  pure $
    GameState
      { _gameBoard = Board (HM.singleton (0, 0) startingTile),
        _gameTiles = initialTiles,
        _gameNumPlayers = numPlayers,
        _gameWhoseTurn = WhoseTurn 0 PhaseTile,
        _gameScores = case numPlayers of NumPlayers n ->
                                           HM.fromList $ take (fromIntegral n) $ zip (PlayerIndex <$> [0..]) (repeat 0)
      }

initAppContext :: (MonadIO m) => m AppContext
initAppContext = do
  gameState <- initGameState (NumPlayers 2)
  pure $
    AppContext
      { _makeTileImageUrl = \(TileImage name ccwRotates) ->
          let suffix = case ccwRotates `mod` 4 of
                0 -> ""
                n -> "-" <> tshow n
           in [txt|/tiles/${name}50${suffix}.jpg|],
        _acGameState = gameState
      }

-- data Message
--   = CurrentTileRotateRight
--   | CurrentTileRotateLeft
--   | PlaceTile (Int, Int)
--   | PlaceMeeple (Int, Int) (Maybe MeeplePlacement)
--   | TakeMeeple (Maybe (Int, Int))

terrainEdges ::
  (HasBoard b) =>
  SideTerrain ->
  (Int, Int) ->
  b ->
  [(TerrainGraphKey, TerrainGraphKey)]
terrainEdges terrain loc board =
  let tile = board ^?! xyToTile . ix loc
      nbrs = tileNeighborhood loc board
      nbrSides = facingSides nbrs
      toMayEdge loc' maySide lrudOne amITerm = case maySide of
        Just nbrTerrain ->
          if traceShowId nbrTerrain /= terrain then Nothing
          else if nbrTerrain /= (tile ^. sides . (toLRUDLens lrudOne))
            then error $
                 "impossible: different terrains neighboring"
                 ++ show loc'
                 ++ show maySide
                 ++ show lrudOne
                 ++ show amITerm
                 ++ show nbrTerrain
                 ++ show terrain
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
            filter snd $ filter (\(t, _) -> t == terrain) $
              sidesWithIsTerminus tile ^.. traverse
      applicableSides =
        filter
          (\s -> terrain == tile ^. sides . (toLRUDLens s))
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

terrainCompleteComponents :: SideTerrain -> GameState -> [[TerrainGraphKey]]
terrainCompleteComponents terrain gs = terrainComponents terrain gs
  & filter (not . HS.member TerrainEmptyKey)
  <&> HS.toList
  -- let terrainAdjMap =
  --       toAdjacencyMap
  --         (Undirected.fromUndirected $ buildTerrainGraph terrain gs)
  --     comps = vertexList $ scc terrainAdjMap
  --  in comps <&> fromNonEmpty
  --       & filter (not . hasVertex TerrainEmptyKey) <&> vertexList

validMeeplePlacements :: (HasBoard a) => (Int, Int) -> a -> [MeeplePlacement]
validMeeplePlacements loc gs =
  let sidePlacements = do
        tile <- gs ^.. board . xyToTile . ix loc
        (lrudOne, sideTerrain) <- toList $ collectLRUDOnes $ tile^.sides
        case sideTerrain of
          Field -> []
          _ -> do
            let tccSets = terrainComponents sideTerrain gs
            myTcc <- traceShowId $ filter (HS.member (TerrainGraphKey loc lrudOne)) tccSets
            let hasMeeple :: TerrainGraphKey -> Bool
                hasMeeple = \case
                  TerrainGraphKey loc' lrudOne' -> fromMaybe False $ do
                    mplace <- gs ^? board . xyToTile . ix loc' . tileMeeplePlacement
                    True <$ mplace
                  TerrainEmptyKey -> False
            if any hasMeeple myTcc && not (HS.member TerrainEmptyKey myTcc)
              then []
              else [PlaceSide lrudOne]
      centerPlacements = do
        tile <- gs ^.. board . xyToTile . ix loc
        case tile^.middle of
          MMonastery -> [PlaceMonastery]
          _ -> []
  in centerPlacements ++ sidePlacements


  -- let [cityGraph, roadGraph] = (`terrainCompleteComponents` gs) <$> [City, Road]



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

collectPlacedMeeple :: TerrainGraphKey -> State GameState (Maybe PlayerIndex)
collectPlacedMeeple key = state $ collectPlacedMeeple' key

data ScorableTerrain = ScoreCity | ScoreRoad deriving (Enum, Bounded)

intoTerrain :: ScorableTerrain -> SideTerrain
intoTerrain ScoreCity = City
intoTerrain ScoreRoad = Road

collectAndScoreMeeples :: GameState -> GameState
collectAndScoreMeeples = execState $ do
  forM_ [minBound..maxBound] $ \terrain -> do
    doneComps <- gets $ terrainCompleteComponents $ intoTerrain terrain
    forM_ (traceShowId doneComps) $ \keyList -> do
      mayPlayerInds <- mapM collectPlacedMeeple keyList
      let playerInds = catMaybes mayPlayerInds
          playerIndsAndCounts = MHM.toList $ foldMap (\i -> MHM.singleton i (Sum 1 :: Sum Int)) playerInds
          winners = maybe [] snd $ (^? ix 0) $ groupSort $ (\(i, Sum c) -> (Max c, i)) <$> playerIndsAndCounts
          multiplier = case terrain of
            ScoreCity -> 2
            ScoreRoad -> 1
      forM_ winners $ \playerInd -> do
        gameScores . ix playerInd += Score (multiplier * genericLength keyList)
  gs <- get
  let allTiles = gs ^. gameBoard . xyToTile . to HM.toList
  forM_ allTiles $ \(loc, tile) -> do
    case tile ^. tileMeeplePlacement of
      Just (PlaceMonastery, playerIndex) -> do
        let nineLocs = do
              dx <- [-1, 0, 1]
              dy <- [-1, 0, 1]
              let (x,y) = loc
                  loc' = (x + dx, y + dy)
              pure loc'
            numFilled = length $ catMaybes $ nineLocs <&> \loc' -> gs ^? gameBoard . xyToTile . ix loc'
        if numFilled == 9 then do
            gameBoard . xyToTile . ix loc . tileMeeplePlacement .= Nothing
            gameScores . ix playerIndex += Score 9
        else pure ()
      _ -> pure ()


reducer CurrentTileRotateRight = gameTiles . ix 0 %~ rotateCcw
reducer CurrentTileRotateLeft = gameTiles . ix 0 %~ rotateCw
reducer (PlaceTile loc) = \gs ->
  let currTile = gs ^?! gameTiles . ix 0
   in gs & gameTiles %~ drop 1
        & gameBoard . xyToTile . at loc ?~ currTile
        -- TODO: check if out of meeples
        & gameWhoseTurn . whoseTurnPhase .~ PhasePlaceMeeple loc
reducer (PlaceMeeple loc mplc) = \gs ->
  let currPlayer = gs ^. gameWhoseTurn . whoseTurnPlayer
   in gs
        & gameBoard . xyToTile . ix loc
          . tileMeeplePlacement
          .~ ((,currPlayer) <$> traceShowId mplc)
        & collectAndScoreMeeples
        -- TODO: check if even has abbot
        & gameWhoseTurn . whoseTurnPhase .~ PhaseTakeAbbot
reducer (TakeAbbot mayLoc) = case mayLoc of
  Nothing -> \gs ->
    let (NumPlayers numPlayers) = gs ^. gameNumPlayers
    in gs & gameWhoseTurn . whoseTurnPhase .~ PhaseTile
          & gameWhoseTurn . whoseTurnPlayer . unPlayerIndex %~ ((`mod` numPlayers) . (+1))
  Just _ -> error "TODO: implement this"

