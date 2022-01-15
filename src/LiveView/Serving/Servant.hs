{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Serving.Servant where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (atomically)

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding
import Debug.Trace
import Import
import LiveView
import LiveView.Html
import LiveView.Serving
import Lucid
import NeatInterpolation
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets             (websocketsOr)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection (withPingThread, Connection)
import Servant hiding (Stream)
import Servant.API.WebSocket
import Servant.Auth.Server.Internal.AddSetCookie
import Servant.HTML.Lucid
import Servant.API.ResponseHeaders
import Streaming
import Streaming.Prelude qualified as S
import Servant.Auth.Server
import Web.Cookie

type LiveViewApi = Get '[HTML] (Html ()) :<|> ("liveview" :> Raw)

data WssUrlSpec
  = Ws
  | Wss

data ScriptData = ScriptData
  { _liveViewScriptAbsolutePath :: T.Text
  , _wssUrlSpec :: WssUrlSpec
  }

data BasePageSpec
  = DefaultBasePage ScriptData
  | CustomBasePage (Html () -> Html ())

defaultBasePage :: T.Text -> ScriptData -> Html () -> Html ()
defaultBasePage rootId (ScriptData liveViewScriptAbsolutePath wssUrlSpec) liveContent = do
   doctypehtml_ $ do
    let scheme = (\case Ws -> "ws:"; Wss -> "wss:") wssUrlSpec
    head_ $ title_ "haskell liveview-simple"
    body_ liveContent
    script_ [type_ "module"] [trimming|
        import {attach} from "$liveViewScriptAbsolutePath";
        (async () => {
          while (true) {
            try {
              const currUrl = new URL(location.href);
              currUrl.protocol = "$scheme";
              let toAdd = '/liveview';
              if (currUrl.pathname.endsWith('/')) {
                toAdd = 'liveview';
              }
              currUrl.pathname = currUrl.pathname + toAdd;
              await attach(document.getElementById("$rootId"), currUrl);
            } catch (e) {
              console.err(e);
            }
          }
        })();
      |]

serveServantLiveView ::
  (String -> IO ()) ->
  BasePageSpec ->
  T.Text ->
  StateStore IO token mutator state ->
  LiveView state mutator ->
  token ->
  Server LiveViewApi
serveServantLiveView debugPrint basePage rootId store lv token =
  initialRenderEndpoint :<|> Tagged (serveWsAsWaiApp liveRenderEndpoint)
  where
    rootWrapper x = div_ [id_ rootId] x
    initialRenderEndpoint = liftIO $ do
      (case basePage of
         DefaultBasePage scriptData -> defaultBasePage rootId scriptData
         CustomBasePage f -> f) <$> init
    liveRenderEndpoint conn = liftIO (live conn)
    (init, _) =
      serveLiveView
        (ServDeps (const (pure ())) debugPrint)
        store
        (rootWrapper lv)
        mempty
        token
    live conn = withPingThread conn 30 (debugPrint "ping") $ sendAll $ snd $
      serveLiveView
        (ServDeps sendMsg debugPrint)
        store
        (rootWrapper lv)
        incomingMsgs
        token
        where
          sendMsg = WS.sendTextData conn
          incomingMsgs =
            S.repeatM (WS.receiveData conn)
          sendAll :: Stream (Of BL.ByteString) IO () -> IO ()
          sendAll = S.mapM_ sendMsg

serveWsAsWaiApp :: (Connection -> IO ()) -> Application
serveWsAsWaiApp f =
  websocketsOr WS.defaultConnectionOptions (WS.acceptRequest >=> f) fallback
  where
    fallback _ respond =
      respond $
      responseLBS
      (mkStatus 426 "Upgrade Required")
      [("Content-Type", "text/plain"),
       ("Upgrade", "WebSocket")]
      "This route is only for WebSocket connections."


-- data AuthWebSocket

-- instance HasServer AuthWebSocket ctx where
--   type ServerT AuthWebSocket m = Connection -> m (Headers '[Header "Set-Cookie" ByteString, Header "Set-Cookie" ByteString] ())
--   hoistServerWithContext _ _ nat svr = nat . svr
--   route Proxy a b = route (Proxy :: Proxy WebSocket) a (_f <$> b)

-- type instance AddSetCookieApi WebSocket = AuthWebSocket

-- instance AddSetCookies ('S ('S 'Z)) (Connection -> Handler ()) 
--   (Connection -> 
--     Handler (Headers '[Header "Set-Cookie" ByteString, Header "Set-Cookie" ByteString] ())) where
--   addSetCookies cookies f conn = case cookies of
--     SetCookieCons (mayA) (SetCookieCons mayB SetCookieNil) -> 
--       let addMayHeader mayH = case mayH of
--               Nothing -> noHeader
--               Just h -> addHeader (toLazyByteString $ renderSetCookie h)
--       in undefined

