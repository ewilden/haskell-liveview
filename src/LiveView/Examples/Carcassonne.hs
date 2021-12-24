{-# LANGUAGE DataKinds #-}
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
  when (me /= player) $ div_ [class_ "current-turn"] $ do
    "Not your turn!"
  when (me == player) $ dimapLiveView id (\m -> gameState %~ reducer m) $
    div_ [class_ "current-turn"] $ do

      case phase of
        PhaseTile -> do
          case tileList of
            [] -> ""
            (currTile : _) -> do
              renderTile currTile ["current-tile"]
              rotLeft <-
                addActionBinding
                "click"
                (const CurrentTileRotateLeft )
              rotRight <-
                addActionBinding
                "click"
                (const CurrentTileRotateRight)
              button_ [hsaction_ rotLeft] "rotate left"
              button_ [hsaction_ rotRight] "rotate right"
        PhasePlaceMeeple loc -> do
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
        PhaseTakeAbbot -> do
          "Take abbot?"
          let mkTakeAbbot mayLoc = addActionBinding "click"
                  (\_ -> TakeAbbot mayLoc)
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
  :<|> ("login" :> ReqBody '[FormUrlEncoded] User :> Post303 '[JSON]
          '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            NoContent)
  :<|> Get '[HTML] (Html ())
  :<|> Raw

-- See https://github.com/haskell-servant/servant-auth/issues/146#issuecomment-660490703

checkCreds :: CookieSettings -> JWTSettings -> ServerContext -> User
           -> Handler (Headers '[ Header "Location" T.Text,
                Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
checkCreds cookieSettings jwtSettings srvCtxt user = do
    wasUnused <- liftIO $ atomically $ do
      userEntry <- StmMap.lookup user (srvCtxt ^. scUserSet)
      case userEntry of
        Nothing -> do
          StmMap.insert () user (srvCtxt ^. scUserSet)
          _mutateState (srvCtxt ^. scStateStore) (SessionId "asdf") (\grc ->
            let currNumPlayers = grc ^. userId2Player . to HM.size
            in  grc & userId2Player . at (UserId $ name user) ?~ PlayerIndex (fromIntegral currNumPlayers))
          pure True
        Just () -> pure False
    if wasUnused then do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
          Nothing           -> throwError err401
          Just applyCookies -> return $ addHeader "/session/asdf" (applyCookies NoContent)
    else throwError err401 { errBody = "Username already taken." }

api :: Proxy (API '[Cookie])
api = Proxy

initServerContext :: (MonadIO m) => m ServerContext
initServerContext = liftIO $ do
  g <- getSplit
  atomically $ flip evalRandT g $ do
    initGRC <- (initGameRoomContext :: RandT StdGen STM GameRoomContext)
    stateStore' <- lift $ inMemoryStateStore (pure initGRC)
    stmm <- lift StmMap.new
    pure $ ServerContext (lmap (intoWithAction .) stateStore') stmm

server' :: CookieSettings -> JWTSettings -> ServerContext -> Server (API auths)
server' cookieSettings jwtSettings servCtxt =
  ( \authRes -> trace (show authRes) $ case authRes of
      Authenticated User { name } -> (\sessId -> serveServantLiveView
        putStrLn
        ( DefaultBasePage $
            ScriptData
              { _liveViewScriptAbsolutePath = "/liveview.js",
                _wssUrlSpec = Ws
              }
        )
        "lvroot"
        (servCtxt ^. stateStore)
        (dimapLiveView (`AppContext` (UserId name))
          (\ f grc -> f (AppContext grc (UserId name)) ^. gameRoomContext)
          liveView)
        (SessionId sessId))
      _ -> const (throwError err401 :<|> undefined)
  )
    :<|> checkCreds cookieSettings jwtSettings servCtxt
    :<|> pure (doctypehtml_ $ do
      form_ [action_ "/login", method_ "POST"] $ do
        div_ $ do
          input_ [name_ "name", placeholder_ "Pick a name"]
        div_ $ button_ "Login"
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
        cookieXsrfSetting = Just defaultXsrfCookieSettings {
          xsrfExcludeGet = True
        }
      }
      cfg = cooks :. jwtCfg :. EmptyContext
  Warp.run 5000 (serveWithContext api cfg (server' defaultCookieSettings jwtCfg servCtxt))
