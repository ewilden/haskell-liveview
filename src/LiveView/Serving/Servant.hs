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
  r
  (Stream (Of r) IO ())
  (LiveView r ())
  (ActionCall -> IO ())

data TeaDeps r msg = TeaDeps 
  r
  (ActionCall -> msg)


serveLVServant :: Handler (ServantLVDeps r) -> Server LiveViewApi
serveLVServant getDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    initialRenderEndpoint = do
      (ServantLVDeps initS stateS lv actionCallback) <- getDeps
      liftIO $ putStrLn "initialRenderEndpoint"
      let initialLiveHtml = _html $ getLiveViewResult initS lv
      pure $ doctypehtml_ $ do
        head_ $ title_ "LiveView test!!"
        body_ $ initialLiveHtml
    liveRenderEndpoint conn = do
      (ServantLVDeps initS stateS lv actionCallback) <- getDeps
      liftIO $ putStrLn "liveRenderEndpoint"
      inpChan <- liftIO STM.newTChanIO
      let writeStatesToInpChan = S.mapM_ (atomically . STM.writeTChan inpChan . DepState) stateS
          writeMessagesToInpChan = forever $ do
            rawMsg <- WS.receiveData conn
            atomically $ STM.writeTChan inpChan $ DepMessage rawMsg
      liftIO $ Async.concurrently_ writeStatesToInpChan
        $ Async.concurrently_ writeMessagesToInpChan
        $ serveLV $ LiveViewDeps
            { _initialState = initS
            , _inputStream = S.repeatM (atomically $ STM.readTChan inpChan)
            , _liveView = lv
            , _sendSocketMessage = WS.sendTextData conn
            , _sendActionCall = actionCallback
            , _debugPrint = const (pure ())
            }
