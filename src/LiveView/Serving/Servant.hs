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
import Streamly qualified as SY
import Streamly.Prelude qualified as SY

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

-- TODO: serve static HTML as the page container!

data ServantDeps = ServantDeps
  { _initialHtml :: Html ()
  , _htmls :: Stream (Of (Html ())) IO ()
  , _actionsCallback :: Stream (Of ActionCall) IO () -> IO ()
  , _basePage :: BasePageSpec
  , _rootId :: T.Text
  }

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
              currUrl.pathname = currUrl.pathname + '/liveview';
              await attach(document.getElementById("$rootId"), currUrl);
            } catch (e) {
              console.err(e);
            }
          }
        })();
      |]

serveLiveViewServant :: Handler ServantDeps -> Server LiveViewApi
serveLiveViewServant getDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    initialRenderEndpoint = do
      (ServantDeps initHtml htmls actionCallback basePage rootId) <- getDeps
      let rootWrapper x = div_ [id_ rootId] x
      liftIO $ putStrLn "initialRenderEndpoint"
      pure $ (case basePage of
        DefaultBasePage scriptData -> defaultBasePage rootId scriptData
        CustomBasePage f -> f) (rootWrapper initHtml)
    liveRenderEndpoint conn = do
      (ServantDeps initHtml htmls actionsCallback mayBasePage rootId) <- getDeps
      let rootWrapper x = div_ [id_ rootId] x
      liftIO $ putStrLn "liveRenderEndpoint"
      let
          rawMsgs = S.repeatM (WS.receiveData conn)
          inpStream = mergePar (S.map (DepHtml . rootWrapper) htmls) (S.map DepMessage rawMsgs)
      liftIO $ withPingThread conn 30 (putStrLn "ping")
        $ actionsCallback $ serveLV $ LiveViewDeps
            { _lvdInitialHtml = rootWrapper initHtml
            , _lvdInputStream = inpStream
            , _lvdSendSocketMessage = WS.sendTextData conn
            , _lvdDebugPrint = putStrLn
            }

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
    live conn = snd $
      serveLiveView
        (ServDeps sendMsg debugPrint)
        store
        (rootWrapper lv)
        incomingMsgs
        token
        where
          sendMsg = WS.sendTextData conn
          incomingMsgs = S.repeatM (WS.receiveData conn)
