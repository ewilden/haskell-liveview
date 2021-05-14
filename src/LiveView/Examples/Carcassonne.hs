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

sampleReducer :: ActionCall -> AppContext -> AppContext
sampleReducer actionCall appContext
  | _action actionCall == "rotate_currTile_left" = appContext & gameTiles . ix 0 %~ rotateCcw
  | _action actionCall == "rotate_currTile_right" = appContext & gameTiles . ix 0 %~ rotateCw
  | _action actionCall == "place_currTile" =
      let currTile = appContext ^?! gameTiles . ix 0
          rawX = _payload actionCall ^?! ix "x"
          rawY = _payload actionCall ^?! ix "y"
          x = read (T.unpack rawX)
          y = read (T.unpack rawY)
      in
        appContext & gameTiles %~ drop 1
        & gameBoard . xyToTile . at (x, y) .~ Just currTile
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


type API = ("session" :> Capture "sessionid" T.Text :> LiveViewApi) :<|> Raw

api :: Proxy API
api = Proxy

initServerContext :: (MonadIO m) => m ServerContext
initServerContext = do
  sessMap <- liftIO StmMap.newIO
  pure (ServerContext sessMap)

initAppContext :: (MonadIO m) => m AppContext
initAppContext = do
  initialTiles <- liftIO $ shuffle unshuffledTiles
  pure $ sampleAppContext & gameTiles .~ initialTiles

getSession :: (MonadIO m) => ServerContext -> T.Text -> m AppContext
getSession servCtxt sessId = liftIO $ do
  let sid = SessionId sessId
  freshAppCtxt <- initAppContext
  STM.atomically $ do
    mayAppCtxt <- StmMap.lookup sid (_sessionMap servCtxt)
    case mayAppCtxt of
      Nothing -> do
        StmMap.insert freshAppCtxt sid (_sessionMap servCtxt)
        pure freshAppCtxt
      Just ac -> pure ac

subscribeSession :: ServerContext -> T.Text -> Stream (Of AppContext) IO ()
subscribeSession servCtxt sessId = do
  let sid = SessionId sessId
  freshAppCtxt <- initAppContext
  liftIO $ STM.atomically $ do
    mayAppCtxt <- StmMap.lookup sid (_sessionMap servCtxt)
    case mayAppCtxt of
      Nothing -> do
        StmMap.insert freshAppCtxt sid (_sessionMap servCtxt)
        pure ()
      Just _ -> pure ()
  undefined

server :: ServerContext -> Server API
server servCtxt = (\sessId -> serveLiveViewServant (do
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
                , _wssUrlSpec = Ws
                })
              "lvroot"
          )) :<|> serveDirectoryWebApp "static"

main :: IO ()
main = do
  servCtxt <- initServerContext
  Warp.run 5000 (serve api (server servCtxt))
