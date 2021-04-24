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
import Import
import Lib
import LiveView.Html
import LiveView.Serving
import LiveView.Serving.Servant
import LiveView.Examples.Carcassonne.Tiles
import LiveView.Examples.Carcassonne.Types
import Lucid
import Lucid.Base (commuteHtmlT)
import Network.Wai.Handler.Warp qualified as Warp
import Servant hiding (Stream)
import Streaming
import Streaming.Prelude qualified as S
import Text.Read

sampleAppContext :: AppContext
sampleAppContext = AppContext
  { _makeTileImageUrl = \(TileImage name ccwRotates) -> 
      let suffix = case ccwRotates of
            0 -> ""
            _ -> let r = tshow ccwRotates in "-" <> r
      in [txt|/tiles/${name}50${suffix}.jpg|]
  , _acGameState = GameState (Board (HM.singleton (0, 0) startingTile)) (fst <$> tileSpecs)
  }

sampleReducer :: ActionCall -> AppContext -> AppContext
sampleReducer actionCall appContext
  | _action actionCall == "rotate_currTile_left" = appContext & gameTiles . ix 0 %~ rotateCcw
  | _action actionCall == "rotate_currTile_right" = appContext & gameTiles . ix 0 %~ rotateCw
  | _action actionCall == "place_currTile" =
      let currTile = appContext ^?! gameTiles . ix 0 in
        undefined
  | otherwise = appContext

sampleLiveView :: HtmlT (Reader AppContext) ()
sampleLiveView = do
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
  renderBoard
  case tileList of
    [] -> ""
    (currTile:_) -> 
      div_ [class_ "current-turn"] $ do
        renderTile currTile ["current-tile"]
        button_ [hsaction_(makeHsaction "click" "rotate_currTile_left")] "rotate left"
        button_ [hsaction_(makeHsaction "click" "rotate_currTile_right")] "rotate right"


type API = LiveViewApi :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveLiveViewServant (do
            let getHtml ac = flip runReader ac $ commuteHtmlT sampleLiveView
            htmlChan <- liftIO STM.newTChanIO
            initialTiles <- liftIO $ shuffle unshuffledTiles
            let initialAppContext = sampleAppContext & gameTiles .~ initialTiles
            appContextTVar <- liftIO $ STM.newTVarIO initialAppContext
            pure $ ServantDeps 
              (getHtml initialAppContext) 
              (S.repeatM (atomically $ STM.readTChan htmlChan)) 
              (S.mapM_ $ \action -> do
                STM.atomically $ do
                  ac <- STM.readTVar appContextTVar 
                  let ac' = sampleReducer action ac
                  STM.writeTVar appContextTVar ac'
                  STM.writeTChan htmlChan (getHtml ac')
                ) 
              (DefaultBasePage $ ScriptData 
                { _liveViewScriptAbsolutePath = "/liveview.js"
                , _wssUrl = "ws://localhost:5000/liveview"
                })
              "lvroot"
          ) :<|> serveDirectoryWebApp "static"

main :: IO ()
main = Warp.run 5000 (serve api server)
