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
import Text.Read
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
  dimapLiveView id (\msg -> gameState %~ reducer msg) renderBoard'
  WhoseTurn player phase <- view gameWhoseTurn
  dimapLiveView id (\m -> gameState %~ reducer m) $ div_ [class_ "current-turn"] $ case phase of
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

newtype User = User { name :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
instance FromForm User

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

checkCreds :: CookieSettings -> JWTSettings -> User
           -> Handler (Headers '[ Header "Location" T.Text,
                Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
checkCreds cookieSettings jwtSettings u@User { name = "Ali Baba" } = do
    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings u
    case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ addHeader (T.pack "/session/asdf") (applyCookies NoContent)
checkCreds _ _ User { name = user } =
        throwError err401

api :: Proxy (API '[Cookie])
api = Proxy

initServerContext :: (MonadIO m) => m ServerContext
initServerContext =
  liftIO $
  ServerContext
    <$> (lmap (intoWithAction .) <$> inMemoryStateStore initGameRoomContext)

server' :: CookieSettings -> JWTSettings -> ServerContext -> Server (API auths)
server' cookieSettings jwtSettings servCtxt = let uid = UserId "foo" in
  ( \authRes -> trace (show authRes) $ case authRes of
      Authenticated _ -> (\sessId -> serveServantLiveView
        putStrLn
        ( DefaultBasePage $
            ScriptData
              { _liveViewScriptAbsolutePath = "/liveview.js",
                _wssUrlSpec = Ws
              }
        )
        "lvroot"
        (servCtxt ^. stateStore)
        (dimapLiveView (`AppContext` uid)
          (\ f grc -> f (AppContext grc uid) ^. gameRoomContext)
          liveView)
        (SessionId sessId))
      _ -> const (throwError err401 :<|> undefined)
  )
    :<|> checkCreds cookieSettings jwtSettings
    :<|> pure (doctypehtml_ $ do
      form_ [action_ "/login", method_ "POST"] $ do
        div_ $ do
          input_ [name_ "name", value_ "Name"]
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
        cookieXsrfSetting = Nothing
      }
      cfg = cooks :. jwtCfg :. EmptyContext
  Warp.run 5000 (serveWithContext api cfg (server' defaultCookieSettings jwtCfg servCtxt))
