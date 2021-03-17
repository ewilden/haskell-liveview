{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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
import Network.WebSockets qualified as WS
import Servant hiding (Stream)
import Servant.API.WebSocket
import Servant.HTML.Lucid
import Streaming
import Streaming.Prelude qualified as S

type LiveViewApi = Get '[HTML] (Html ()) :<|> "liveview" :> WebSocket

-- TODO: serve static HTML as the page container!

data ServantLVDeps r = ServantLVDeps
  { _initialState :: r
  , _states :: Stream (Of r) IO ()
  , _liveView :: LiveView r ()
  , _actionsCallback :: Stream (Of ActionCall) IO () -> IO ()
  , _basePage :: Maybe (Html () -> Html ())
  }

defaultBasePage :: Html () -> Html ()
defaultBasePage liveContent = do
   doctypehtml_ $ do
    head_ $ title_ "haskell liveview-simple"
    body_ liveContent
    script_ [src_ "index.js"] $ T.pack ""

serveLVServant :: Handler (ServantLVDeps r) -> Server LiveViewApi
serveLVServant getDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    rootWrapper x = div_ [id_ "lvroot"] x
    initialRenderEndpoint = do
      (ServantLVDeps initS stateS lv actionCallback mayBasePage) <- getDeps
      liftIO $ putStrLn "initialRenderEndpoint"
      let initialLiveHtml = rootWrapper $ _html $ getLiveViewResult initS lv
      pure $ fromMaybe defaultBasePage mayBasePage initialLiveHtml
    liveRenderEndpoint conn = do
      (ServantLVDeps initS stateS lv actionsCallback mayBasePage) <- getDeps
      liftIO $ putStrLn "liveRenderEndpoint"
      inpChan <- liftIO STM.newTChanIO
      let writeStatesToInpChan = S.mapM_ (atomically . STM.writeTChan inpChan . DepState) stateS
          writeMessagesToInpChan = forever $ do
            rawMsg <- WS.receiveData conn
            atomically $ STM.writeTChan inpChan $ DepMessage rawMsg
      liftIO $ Async.race_ writeStatesToInpChan
        $ Async.race_ writeMessagesToInpChan
        $ actionsCallback $ serveLV $ LiveViewDeps
            { _lvdInitialState = initS
            , _lvdInputStream = S.repeatM (atomically $ STM.readTChan inpChan)
            , _lvdLiveView = lv
            , _lvdSendSocketMessage = WS.sendTextData conn
            , _lvdRootWrapper = rootWrapper
            , _lvdDebugPrint = putStrLn
            }
