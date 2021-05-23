{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Serving.Servant where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (atomically)

import Control.Lens
import Data.Aeson
import Data.Text qualified as T
import Data.Text.Encoding
import Debug.Trace
import Import
import LiveView
import LiveView.Html
import LiveView.Serving
import Lucid
import NeatInterpolation
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection (withPingThread)
import Servant hiding (Stream)
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Streaming
import Streaming.Prelude qualified as S

type LiveViewApi = Get '[HTML] (Html ()) :<|> "liveview" :> WebSocket

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
  StateStore token state ->
  LiveView state ->
  token ->
  Server LiveViewApi
serveServantLiveView debugPrint basePage rootId store lv token =
  initialRenderEndpoint :<|> liveRenderEndpoint
  where
    rootWrapper x = div_ [id_ rootId] x
    initialRenderEndpoint = liftIO $ do
      initHtml <- init
      pure $ (case basePage of
                DefaultBasePage scriptData -> defaultBasePage rootId scriptData
                CustomBasePage f -> f) initHtml
    liveRenderEndpoint conn = liftIO (live conn)
    (init, _) =
      serveLiveView
        (ServDeps (const (pure ())) debugPrint)
        store
        (rootWrapper lv)
        mempty
        token
    live conn = withPingThread conn 30 (debugPrint "ping") $ snd $
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
