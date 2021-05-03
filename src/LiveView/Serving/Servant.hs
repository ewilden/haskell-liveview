-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
