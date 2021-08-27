{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne where

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
import LiveView.Examples.Carcassonne.Reducer
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

liveView :: LiveView AppContext (AppContext -> AppContext)
liveView = do
  link_ [rel_ "stylesheet", href_ "/carcassonne.css"]
  tileList <- view (gameState . gameTiles)
  mkUrl <- view makeTileImageUrl
  let currTileProp = case tileList of
        [] -> ""
        (currTile : _) ->
          let url = mkUrl (_image currTile)
           in [txt|--curr-tile-img: url('${url}');|]
  style_
    [txt|
    :root {
      --tile-hl-color: gold;
      --tile-size: 80px;
      $currTileProp
    }
    |]
  dimapLiveView id (\msg -> gameState %~ reducer msg) renderBoard'
  WhoseTurn player phase <- view gameWhoseTurn
  div_ [class_ "current-turn"] $ case phase of
    PhaseTile -> do
      case tileList of
        [] -> ""
        (currTile : _) -> do
          renderTile currTile ["current-tile"]
          rotLeft <-
            addActionBinding
             "click"
             (\_ -> gameTiles . ix 0 %~ rotateCcw)
          rotRight <-
            addActionBinding
            "click"
            (\_ -> gameTiles . ix 0 %~ rotateCw)
          button_ [hsaction_ rotLeft] "rotate left"
          button_ [hsaction_ rotRight] "rotate right"
    PhasePlaceMeeple loc -> dimapLiveView id (\m -> gameState %~ reducer m) $ do
      "Place meeple?"
      -- TODO: surface which meeple placements are valid
      let mkPlaceMeeple mayPlace = addActionBinding "click"
              (\_ -> PlaceMeeple loc mayPlace)
      ul_ $ do
        gs <- view gameState
        forM_ (validMeeplePlacements loc gs) $ \plc -> li_ $ do
          plcAction <- mkPlaceMeeple (Just plc)
          button_ [hsaction_ plcAction] $ fromString $ show $ plc
      noPlaceMeeple <- mkPlaceMeeple Nothing
      button_ [hsaction_ noPlaceMeeple] "skip"
    PhaseTakeAbbot -> dimapLiveView id (\m -> gameState %~ reducer m) $ do
      "Take abbot?"
      let mkTakeAbbot mayLoc = addActionBinding "click"
              (\_ -> TakeAbbot mayLoc)
      noTakeAbbot <- mkTakeAbbot Nothing
      button_ [hsaction_ noTakeAbbot] "skip"
  div_ $ do
    gs <- view gameState
    fromString $ show $ gs {
                            _gameTiles = [],
                            _gameBoard = Board mempty
                           }

type API = ("session" :> Capture "sessionid" T.Text :> LiveViewApi) :<|> Raw

api :: Proxy API
api = Proxy

initServerContext :: (MonadIO m) => m ServerContext
initServerContext = do
  liftIO $
    ServerContext
      <$> (lmap (intoWithAction .) <$> inMemoryStateStore initAppContext)

server' :: ServerContext -> Server API
server' servCtxt =
  ( serveServantLiveView
      putStrLn
      ( DefaultBasePage $
          ScriptData
            { _liveViewScriptAbsolutePath = "/liveview.js",
              _wssUrlSpec = Ws
            }
      )
      "lvroot"
      (servCtxt ^. stateStore)
      liveView
      . SessionId
  )
    :<|> serveDirectoryWebApp "static"

main :: IO ()
main = do
  servCtxt <- initServerContext
  Warp.run 5000 (serve api (server' servCtxt))
