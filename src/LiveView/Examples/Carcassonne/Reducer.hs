{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne.Reducer where

import Algebra.Graph
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.Composition ((.:))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.String (fromString)
import Data.Text qualified as T
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
import Streaming
import Streaming.Prelude qualified as S
import Text.Read

initGameState :: (MonadIO m) => NumPlayers -> m GameState
initGameState numPlayers = do
  initialTiles <- liftIO $ shuffle unshuffledTiles
  pure $
    GameState
      { _gameBoard = Board (HM.singleton (0, 0) startingTile),
        _gameTiles = initialTiles,
        _gameNumPlayers = numPlayers,
        _gameWhoseTurn = WhoseTurn 0 PhaseTile,
        _gameScores = mempty
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

terrainNeighborhood ::
  (HasBoard b) => SideTerrain -> TerrainGraphKey -> b -> [TerrainGraphKey]
terrainNeighborhood terrain key board =
  let loc = key ^. keyLoc
      nbrs = tileNeighborhood loc board
      nbrSides = facingSides nbrs
      toMayKey loc' maySide lrudOne =
        if maySide == Just terrain
          then
            Just $
              TerrainGraphKey
                { _keyLoc = loc',
                  _keySide = lrudOne
                }
          else Nothing
   in catMaybes $
        ( toMayKey
            <$> (lrudNeighbors <*> pure loc)
            <*> nbrSides
            <*> (flipLRUDOne <$> lrudOnes)
        )
          ^.. traverse

buildTerrainGraph :: SideTerrain -> GameState -> Graph TerrainGraphKey
buildTerrainGraph terrain gs = HM.foldlWithKey' folder empty (gs ^. xyToTile)
  where
    folder graph loc tile = overlay graph graph'
    graph' = undefined

reducer :: Message -> GameState -> GameState
reducer CurrentTileRotateRight = gameTiles . ix 0 %~ rotateCcw
reducer CurrentTileRotateLeft = gameTiles . ix 0 %~ rotateCw
reducer (PlaceTile loc) = \gs ->
  let currTile = gs ^?! gameTiles . ix 0
   in gs & gameTiles %~ drop 1
        & gameBoard . xyToTile . at loc ?~ currTile
        & gameWhoseTurn . whoseTurnPhase %~ succ
reducer (PlaceMeeple loc mplc) = \gs ->
  let currPlayer = gs ^. gameWhoseTurn . whoseTurnPlayer
   in gs
        & gameBoard . xyToTile . ix loc
          . tileMeeplePlacement
          .~ ((,currPlayer) <$> mplc)
        & collectAndScoreMeeples
        & gameWhoseTurn . whoseTurnPhase %~ succ
reducer (TakeAbbot mayLoc) = case mayLoc of
  Nothing -> id
  Just _ -> error "TODO: implement this"

collectAndScoreMeeples :: GameState -> GameState
collectAndScoreMeeples = undefined
