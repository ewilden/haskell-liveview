{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State
import Crypto.JOSE.JWK
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Composition ((.:))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.String (fromString)
import Data.Text qualified as T
import Debug.Trace
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
import Servant.Auth.Server
import Servant.HTML.Lucid
import StmContainers.Map qualified as StmMap
import Streaming
import Streaming.Prelude qualified as S
import Web.FormUrlEncoded
import Web.Simple.Responses (forbidden)

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
  div_ $ do
    (UserId uid) <- view userId
    fromString $ "Logged in as " <> T.unpack uid
  dimapLiveView id (\msg -> gameState %~ reducer msg) renderBoard'
  WhoseTurn player phase <- view gameWhoseTurn
  me <- asks myPlayerIndex
  when (me /= player) $ div_ [class_ "current-turn"] $
    "Not your turn!"
  when (me == player) $ dimapLiveView id (\m -> gameState %~ reducer m) $
    div_ [class_ "current-turn"] $
      case phase of
        PhaseTile -> do
          case tileList of
            [] -> ""
            (currTile : _) -> do
              renderTile currTile ["current-tile"]
              rotLeft <-
                addActionBinding
                "click"
                (const CurrentTileRotateLeft)
              rotRight <-
                addActionBinding
                "click"
                (const CurrentTileRotateRight)
              button_ [hsaction_ rotLeft] "rotate left"
              button_ [hsaction_ rotRight] "rotate right"
        PhasePlaceMeeple loc -> do
          "Place meeple?"
          let mkPlaceMeeple mayPlace = addActionBinding "click"
                  (\_ -> PlaceMeeple loc mayPlace)
          ul_ $ do
            gs <- view gameState
            forM_ (validMeeplePlacements loc gs) $ \plc -> li_ $ do
              plcAction <- mkPlaceMeeple (Just plc)
              button_ [hsaction_ plcAction] $ fromString $ show plc
          noPlaceMeeple <- mkPlaceMeeple Nothing
          button_ [hsaction_ noPlaceMeeple] "skip"
        PhaseTakeAbbot -> do
          "Take abbot?"
          gs <- view gameState
          me <- asks myPlayerIndex
          let mkTakeAbbot mayLoc = addActionBinding "click"
                  (\_ -> TakeAbbot mayLoc)
              mayAbbot = flip ifind (gs ^. xyToTile) $ \loc tile -> case tile ^. tileMeeplePlacement of
                Nothing -> False
                Just (PlaceAbbot, pi) -> pi == me
                _ -> False
          case mayAbbot of
            Just (loc, _) -> do
              takeAbbot <- mkTakeAbbot $ Just loc
              button_ [hsaction_ takeAbbot] "take abbot"
            Nothing -> pure ()
          noTakeAbbot <- mkTakeAbbot Nothing
          button_ [hsaction_ noTakeAbbot] "skip"
        PhaseGameOver -> "Game over!"
  div_ $ do
    gs <- view gameState
    fromString $ show $ gs {
                            _gameTiles = [],
                            _gameBoard = Board mempty
                           }

type Post303 (cts :: [*]) (hs :: [*]) a =
  Verb 'POST 303 cts (Headers (Header "Location" T.Text ': hs) a)

type API auths
  = (Auth auths User :> "session" :> Capture "sessionid" T.Text :> LiveViewApi)
  :<|> (Auth auths User :> "join" :> ReqBody '[FormUrlEncoded] SessionId :> Post303 '[JSON] '[] NoContent)
  :<|> ("login" :> ReqBody '[FormUrlEncoded] User :> Post303 '[JSON]
          '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            NoContent)
  :<|> Get '[HTML] (Html ())
  :<|> "lobby" :> Get '[HTML] (Html ())
  :<|> Raw

-- See https://github.com/haskell-servant/servant-auth/issues/146#issuecomment-660490703

joinSession :: ServerContext -> AuthResult User -> SessionId -> Handler (Headers '[ Header "Location" T.Text] NoContent)
joinSession servCtxt authResult (SessionId sessId) = case authResult of
  Authenticated User { name } -> do
    eith <- liftIO $ runRandSTMIntoIO $ do
      (curr, _) <- (servCtxt ^. stateStore . subscribeState) $ SessionId sessId
      let sessNumPlayers = curr ^. userId2Player . to HM.size
      if sessNumPlayers < 5
          then do
            _mutateState (servCtxt ^. stateStore) (SessionId sessId) $ userId2Player . at (UserId name) ?~ PlayerIndex (fromIntegral sessNumPlayers)
            pure $ Right ()
          else pure $ Left err403
      pure $ Right ()
    case eith of
      Right () -> pure ()
      Left e -> throwError e
    pure $ (addHeader ("/session/" <> sessId) NoContent)
  _ -> do
    liftIO $ print "Failed to auth joinSession"
    throwError err401

takeUsername :: CookieSettings -> JWTSettings -> ServerContext -> User
           -> Handler (Headers '[ Header "Location" T.Text,
                Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
takeUsername cookieSettings jwtSettings srvCtxt user = do
    maySessid <- liftIO $ runRandSTMIntoIO $ do
      userEntry <- lift $ StmMap.lookup user (srvCtxt ^. scUserSet)
      case userEntry of
        Nothing -> do
          let getsessid = do
                candidate <- getRandom <&> abs <&> fromString @T.Text . take 16 . show @Int
                exists <- srvCtxt ^. scStateStore . existsState $ SessionId candidate
                if exists then getsessid else pure candidate
          sessid <- getsessid
          lift $ StmMap.insert () user (srvCtxt ^. scUserSet)
          _mutateState (srvCtxt ^. scStateStore) (SessionId sessid) (\grc ->
            let currNumPlayers = grc ^. userId2Player . to HM.size
            in  grc & userId2Player . at (UserId $ name user) ?~ PlayerIndex (fromIntegral currNumPlayers))
          pure (Just sessid)
        Just () -> pure Nothing
    case maySessid of
      Nothing -> throwError err401 { errBody = "Username already taken." }
      Just sessid -> do
        mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
        case mApplyCookies of
            Nothing           -> throwError err401
            Just applyCookies -> return $ addHeader ("/session/" <> sessid) (applyCookies NoContent)
-- TODO: allow user to join game created by someone else

api :: Proxy (API '[Cookie])
api = Proxy

initServerContext :: (MonadIO m) => m ServerContext
initServerContext =
  liftIO $ runIntoIO $ do
  stateStore' <- inMemoryStateStore lift (initGameRoomContext @(RandT StdGen STM))
  stmm <- lift StmMap.new
  pure $ ServerContext (lmap (intoWithAction .) stateStore') stmm
  where runIntoIO ma = do
          g <- getSplit
          atomically $ evalRandT ma g

server' :: CookieSettings -> JWTSettings -> ServerContext -> Server (API auths)
server' cookieSettings jwtSettings servCtxt =
  ( \case
      Authenticated User { name } -> (serveServantLiveView
        putStrLn
        ( DefaultBasePage $
            ScriptData
              { _liveViewScriptAbsolutePath = "/liveview.js",
                _wssUrlSpec = Ws
              }
        )
        "lvroot"
        (hoistM runRandSTMIntoIO $ servCtxt ^. stateStore)
        (dimapLiveView (`AppContext` UserId name)
          (\ f grc -> f (AppContext grc (UserId name)) ^. gameRoomContext)
          liveView) . SessionId)
      _ -> const (throwError err401 :<|> Tagged (\req resp -> resp forbidden))
  ) :<|> joinSession servCtxt
    :<|> takeUsername cookieSettings jwtSettings servCtxt
    :<|> pure (doctypehtml_ $
                form_ [action_ "/login", method_ "POST"] $ do
                  div_ $ do
                    input_ [name_ "name", placeholder_ "Pick a name"]
                  div_ $ button_ "Login"
                      )
    :<|> do 
            listStream <- liftIO $ runRandSTMIntoIO $ servCtxt ^. stateStore . allStates
            stateList <- fromJust <$> liftIO (runRandSTMIntoIO $ S.head_ listStream)
            pure (doctypehtml_ $ do
                form_ [action_ "/join", method_ "POST"] $ do
                  div_ $ do
                    input_ [name_ "sessionId", placeholder_ "Session ID"]
                  div_ $ button_ "Join"
                form_ [action_ "/join", method_ "POST"] $ ul_ $ do
                  forM_ stateList $ \entry -> do
                    let sessid = sessionId $ entry ^. _1
                    input_ [type_ "radio", name_ "sessionId", value_ sessid, id_ sessid]
                    label_ [for_ sessid] $ toHtml sessid
                    -- fromString $ show (fst entry, entry ^. _2 . userId2Player)
                  div_ $ button_ "Join"
                )
    :<|> serveDirectoryWebApp "static"

main :: IO ()
main = do
  servCtxt <- initServerContext
  jwk <- generateKey
  let jwtCfg = defaultJWTSettings jwk
      cooks = defaultCookieSettings {
        cookieIsSecure = NotSecure,
        cookieSameSite = SameSiteStrict,
        cookieXsrfSetting = Nothing
        -- cookieXsrfSetting = Just defaultXsrfCookieSettings {
        --   xsrfExcludeGet = True
        -- }
      }
      cfg = cooks :. jwtCfg :. EmptyContext
  Warp.run 5000 (serveWithContext api cfg (server' defaultCookieSettings jwtCfg servCtxt))
