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

data ScriptData = ScriptData 
  { _liveViewScriptAbsolutePath :: T.Text
  , _wssUrl :: T.Text
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
defaultBasePage rootId (ScriptData liveViewScriptAbsolutePath wssUrl) liveContent = do
   doctypehtml_ $ do
    head_ $ title_ "haskell liveview-simple"
    body_ liveContent
    script_ [type_ "module"] [trimming|
        import {attach} from "$liveViewScriptAbsolutePath";
        (async () => {
          while (true) {
            try {
              await attach(document.getElementById("$rootId"), "$wssUrl");
            } catch (e) {
              // pass
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
      inpChan <- liftIO STM.newTChanIO
      let writeHtmlsToInpChan = S.mapM_ (atomically . STM.writeTChan inpChan . DepHtml . rootWrapper) htmls
          writeMessagesToInpChan = withPingThread conn 30 (pure ()) $ forever $ do
            rawMsg <- WS.receiveData conn
            atomically $ STM.writeTChan inpChan $ DepMessage rawMsg
      liftIO $ Async.race_ writeHtmlsToInpChan
        $ Async.race_ writeMessagesToInpChan
        $ actionsCallback $ serveLV $ LiveViewDeps
            { _lvdInitialHtml = rootWrapper initHtml
            , _lvdInputStream = S.repeatM (atomically $ STM.readTChan inpChan)
            , _lvdSendSocketMessage = WS.sendTextData conn
            , _lvdDebugPrint = putStrLn
            }
