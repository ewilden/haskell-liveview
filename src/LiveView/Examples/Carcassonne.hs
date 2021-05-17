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
import Data.Hashable (Hashable)

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
  liftIO $ ServerContext <$> StmMap.newIO

initAppContext :: (MonadIO m) => m AppContext
initAppContext = do
  initialTiles <- liftIO $ shuffle unshuffledTiles
  pure $ sampleAppContext & gameTiles .~ initialTiles

getOrInit :: (Eq k, Ord k, Hashable k) => STM.STM a -> k -> StmMap.Map k a -> STM.STM a
getOrInit def k m = do
  mayV <- StmMap.lookup k m
  case mayV of
    Nothing -> do
      v <- def
      StmMap.insert v k m >> pure v
    Just v -> pure v

getOrInitM :: (MonadIO m, Eq k, Ord k, Hashable k) => m a -> k -> StmMap.Map k a -> m a
getOrInitM mdef k m = do
  def <- mdef
  liftIO $ STM.atomically $ getOrInit (pure def) k m

freshSessState :: (MonadIO m) => m (SessionState AppContext)
freshSessState = SessionState <$> liftIO STM.newBroadcastTChanIO <*> initAppContext

getSession :: (MonadIO m) => ServerContext -> T.Text -> m AppContext
getSession servCtxt sessId = liftIO $ fst <$> subscribeSession servCtxt sessId

mutateSession :: (MonadIO m) => ServerContext -> T.Text -> (AppContext -> AppContext) -> m ()
mutateSession servCtxt sessId f = liftIO $ do
  let sid = SessionId sessId
  STM.atomically $ do
    mayAppCtxt <- StmMap.lookup sid (_sessionMap servCtxt)
    case mayAppCtxt of
      Nothing -> pure ()
      Just (SessionState chan ac) -> do
        let newAC = f ac
        STM.writeTChan chan $ Just newAC
        StmMap.insert (SessionState chan newAC) sid (_sessionMap servCtxt)

subscribeSession :: ServerContext -> T.Text -> IO (AppContext, Stream (Of AppContext) IO ())
subscribeSession servCtxt sessId = do
  let sid = SessionId sessId
  SessionState wChan ac <- getOrInitM freshSessState sid (_sessionMap servCtxt)
  chan <- atomically $ STM.dupTChan wChan
  pure (ac, S.untilRight $ maybe (Right ()) Left <$> STM.atomically (STM.readTChan chan))



server :: ServerContext -> Server API
server servCtxt = (\sessId -> serveLiveViewServant (do
            let getHtml ac = flip runReader ac $ commuteHtmlT sampleLiveView
            htmlChan <- liftIO STM.newTChanIO
            initialTiles <- liftIO $ shuffle unshuffledTiles
            let initialAppContext = sampleAppContext & gameTiles .~ initialTiles
            (initialAppContext, acStream) <- liftIO $ subscribeSession servCtxt sessId
            pure $ ServantDeps
              (getHtml initialAppContext)
              (S.map getHtml acStream)
              (S.mapM_ $ \action -> do
                mutateSession servCtxt sessId (sampleReducer action)
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
