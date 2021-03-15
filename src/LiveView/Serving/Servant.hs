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

data ServantDeps r m = ServantDeps 
  { _subscribeToState :: m (r, Stream (Of r) IO (), ActionCall -> m ())
  -- , _actionCallback :: ActionCall -> m ()
  }

-- TODO: serve static HTML as the page container!

data ServantLVDeps r = ServantLVDeps
  r
  (Stream (Of r) IO ())
  (LiveView r ())
  (ActionCall -> IO ())

serveLVServant :: IO (ServantLVDeps r) -> Server LiveViewApi
serveLVServant getDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    initialRenderEndpoint = do
      (ServantLVDeps initS stateS lv actionCallback) <- liftIO getDeps
      liftIO $ putStrLn "initialRenderEndpoint"
      let initialLiveHtml = _html $ getLiveViewResult initS lv
      pure $ doctypehtml_ $ do
        head_ $ title_ "LiveView test!!"
        body_ $ initialLiveHtml
    liveRenderEndpoint conn = do
      (ServantLVDeps initS stateS lv actionCallback) <- liftIO getDeps
      liftIO $ putStrLn "liveRenderEndpoint"
      inpChan <- liftIO STM.newTChanIO
      let writeStatesToInpChan = S.mapM_ (atomically . STM.writeTChan inpChan . DepState) stateS
          writeMessagesToInpChan = forever $ do
            rawMsg <- WS.receiveData conn
            atomically $ STM.writeTChan inpChan $ DepMessage rawMsg
      liftIO $ Async.concurrently_ writeStatesToInpChan
        $ Async.concurrently_ writeMessagesToInpChan
        $ serveLV $ LiveViewDeps
            { _initstate = initS
            , _depStream = S.repeatM (atomically $ STM.readTChan inpChan)
            , _liveview = lv
            , _sendSocketMessage = WS.sendTextData conn
            , _sendActionCall = actionCallback
            , _debPrint = putStrLn
            }


serveLiveViewServant :: forall r m. (MonadIO m, m ~ IO) => LiveView r () -> ServantDeps r m -> ServerT LiveViewApi m
serveLiveViewServant liveView servDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    initialRenderEndpoint = do
      liftIO $ putStrLn "initialRenderEndpoint"
      initialState <- (^. _1) <$> _subscribeToState servDeps
      let initialLiveHtml = _html $ getLiveViewResult initialState liveView
      pure $ doctypehtml_ $ do
        head_ $ title_ "LiveView test!!"
        body_ $ initialLiveHtml
    liveRenderEndpoint conn = do
      liftIO $ putStrLn "liveRenderEndpoint"
      (initS, states, actionCallback) <- _subscribeToState servDeps
      liftIO $ putStrLn "after _subscribeToState call"
      inpChan <- liftIO STM.newTChanIO
      let actionThread = forever $ do
            receivedMsg <- WS.receiveData conn
            Prelude.putStrLn $ show receivedMsg
            case (decodeStrict receivedMsg :: Maybe (ActionCall, Clock)) of
              Nothing -> Prelude.putStrLn "failed to decode"
              Just actionCall -> do
                Prelude.putStrLn "parsed successfully"
                Prelude.putStrLn $ show actionCall
                STM.atomically $ STM.writeTChan inpChan (InputAction actionCall)
                Prelude.putStrLn "wrote to chan"
          stateThread = S.mapM_ (\s -> do
            putStrLn "in stateThread"
            STM.atomically $ STM.writeTChan inpChan $ InputState s
            Prelude.putStrLn "wrote state") states
      liftIO $ Async.withAsync actionThread $ \_ -> do
        liftIO $ Async.withAsync stateThread $ \_ -> do
          let inputStream :: Stream (Of (InputStreamEntry r)) m ()
              inputStream = S.repeatM (liftIO $ do
                putStrLn "reading inpChan into inputStream"
                x <- STM.atomically $ STM.readTChan inpChan
                putStrLn "successfully read"
                pure x)
              inputs = LiveViewInputs initS inputStream
              outputs = serveLiveView liveView inputs
              mountMsg = ((T.pack "mount", fst $ _mountList outputs), snd $ _mountList outputs)
          liftIO $ WS.sendTextData conn $ encode mountMsg
          liftIO $ putStrLn "about to mapM_ over outputStream"
          flip S.mapM_ (_outputStream outputs) $ \case
              OutputAction call -> do
                putStrLn "calling OutputAction"
                actionCallback call
              OutputPatch (patchList, clock) -> liftIO $ WS.sendTextData conn $ encode ((T.pack "patch", patchList), clock)
