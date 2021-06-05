{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne.Reducer where

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
          let suffix = case ccwRotates of
                0 -> ""
                _ -> let r = tshow ccwRotates in "-" <> r
           in [txt|/tiles/${name}50${suffix}.jpg|],
        _acGameState = gameState
      }
