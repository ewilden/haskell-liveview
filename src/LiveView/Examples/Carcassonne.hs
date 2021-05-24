{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (atomically)
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.String (fromString)
import Data.Text qualified as T
import Focus qualified
import Import
import Lib
import ListT qualified
import LiveView
import LiveView.Html
import LiveView.Serving
import LiveView.Serving.Servant
import LiveView.Examples.Carcassonne.Tiles
import LiveView.Examples.Carcassonne.Types
import Lucid
import Lucid.Base (commuteHtmlT)
import Network.Wai.Handler.Warp qualified as Warp
import Servant hiding (Stream)
import StmContainers.Map qualified as StmMap
import Streaming
import Streaming.Prelude qualified as S
import Text.Read
import Data.Hashable (Hashable)
import Data.Composition ((.:))

sampleAppContext :: AppContext
sampleAppContext = AppContext
  { _makeTileImageUrl = \(TileImage name ccwRotates) ->
      let suffix = case ccwRotates of
            0 -> ""
            _ -> let r = tshow ccwRotates in "-" <> r
      in [txt|/tiles/${name}50${suffix}.jpg|]
  , _acGameState = GameState (Board (HM.singleton (0, 0) startingTile)) (fst <$> tileSpecs)
  , _sessionId = "sampleAppContext"
  }

liveView :: LiveView AppContext (AppContext -> WithAction IO AppContext)
liveView = do
  link_ [rel_ "stylesheet", href_ "/carcassonne.css"]
  tileList <- view (gameState . gameTiles)
  mkUrl <- view makeTileImageUrl
  let currTileProp = case tileList of
        [] -> ""
        (currTile:_) -> let url = mkUrl (_image currTile) in
          [txt|--curr-tile-img: url('${url}');|]
  style_ [txt|
    :root {
      --tile-hl-color: gold;
      --tile-size: 80px;
      $currTileProp
    }
    |]
  renderBoard'
  case tileList of
    [] -> ""
    (currTile:_) ->
      div_ [class_ "current-turn"] $ do
        renderTile currTile ["current-tile"]
        rotLeft <- addActionBinding "click"
          (\_ -> (intoWithAction .) $ gameTiles . ix 0 %~ rotateCcw)
        rotRight <- addActionBinding "click"
          (\_ -> (intoWithAction .) $ gameTiles . ix 0 %~ rotateCw)
        button_ [hsaction_ rotLeft] "rotate left"
        button_ [hsaction_ rotRight] "rotate right"


type API = ("session" :> Capture "sessionid" T.Text :> LiveViewApi) :<|> Raw

api :: Proxy API
api = Proxy

initServerContext :: (MonadIO m) => m ServerContext
initServerContext = do
  liftIO $ ServerContext <$> inMemoryStateStore initAppContext

initAppContext :: (MonadIO m) => m AppContext
initAppContext = do
  initialTiles <- liftIO $ shuffle unshuffledTiles
  pure $ sampleAppContext & gameTiles .~ initialTiles

server' :: ServerContext -> Server API
server' servCtxt = (serveServantLiveView
                   putStrLn
                   (DefaultBasePage $ ScriptData
                    { _liveViewScriptAbsolutePath = "/liveview.js"
                    , _wssUrlSpec = Ws
                    })
                   "lvroot"
                   (servCtxt ^. stateStore)
                   liveView . SessionId) :<|> serveDirectoryWebApp "static"

main :: IO ()
main = do
  servCtxt <- initServerContext
  Warp.run 5000 (serve api (server' servCtxt))
