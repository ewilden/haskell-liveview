{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LiveView.Serving.Servant where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM

import Control.Lens
import Data.Aeson
import Data.Text qualified as T
import Data.Text.Encoding
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

serveLiveViewServant :: forall r m. (MonadIO m, m ~ IO) => LiveView r () -> ServantDeps r m -> ServerT LiveViewApi m
serveLiveViewServant liveView servDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    initialRenderEndpoint = do
      initialState <- (^. _1) <$> _subscribeToState servDeps
      let initialLiveHtml = _html $ getLiveViewResult initialState liveView
      pure $ doctypehtml_ $ do
        head_ $ title_ "LiveView test!!"
        body_ $ initialLiveHtml
    liveRenderEndpoint conn = do
      (initS, states, actionCallback) <- _subscribeToState servDeps
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
            STM.atomically $ STM.writeTChan inpChan $ InputState s
            Prelude.putStrLn "wrote state") states
      liftIO $ Async.withAsync actionThread $ \_ -> do
        liftIO $ Async.withAsync stateThread $ \_ -> do
          let inputStream :: Stream (Of (InputStreamEntry r)) m ()
              inputStream = S.repeatM (liftIO $ STM.atomically $ STM.readTChan inpChan)
              inputs = LiveViewInputs initS inputStream
              outputs = serveLiveView liveView inputs
              mountMsg = ((T.pack "mount", fst $ _mountList outputs), snd $ _mountList outputs)
          liftIO $ WS.sendTextData conn $ encode mountMsg
          flip S.mapM_ (_outputStream outputs) $ \case
              OutputAction call -> do
                putStrLn "calling OutputAction"
                actionCallback call
              OutputPatch (patchList, clock) -> liftIO $ WS.sendTextData conn $ encode ((T.pack "patch", patchList), clock)
