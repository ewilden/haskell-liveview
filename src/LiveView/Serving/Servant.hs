{-# LANGUAGE DataKinds #-}
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

serveLiveViewServant :: forall r m. (MonadIO m) => LiveView r () -> ServantDeps r m -> ServerT LiveViewApi m
serveLiveViewServant liveView servDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    initialRenderEndpoint = do
      initialState <- (^. _1) <$> _subscribeToState servDeps
      pure $ _html $ getLiveViewResult initialState liveView
    liveRenderEndpoint conn = do
      (initS, states, actionCallback) <- _subscribeToState servDeps
      inpChan <- liftIO STM.newTChanIO
      let actionThread = forever $ do
            receivedMsg <- WS.receiveData conn
            Prelude.putStrLn $ show receivedMsg
            case (decodeStrict receivedMsg :: Maybe (ActionCall, Clock)) of
              Nothing -> pure ()
              Just actionCall -> STM.atomically $ STM.writeTChan inpChan (InputAction actionCall)
          stateThread = S.mapM_ (STM.atomically . STM.writeTChan inpChan . InputState) states
      liftIO $ Async.withAsync actionThread $ \_ -> pure ()
      liftIO $ Async.withAsync stateThread $ \_ -> pure ()
      let inputStream :: Stream (Of (InputStreamEntry r)) m ()
          inputStream = S.repeatM (liftIO $ STM.atomically $ STM.readTChan inpChan)
          inputs = LiveViewInputs initS inputStream
          outputs = serveLiveView liveView inputs
          mountMsg = ((T.pack "mount", fst $ _mountList outputs), snd $ _mountList outputs)
      liftIO $ WS.sendTextData conn $ encode mountMsg
      flip S.mapM_ (_outputStream outputs) $ \case
          OutputAction call -> actionCallback call
          OutputPatch (patchList, clock) -> liftIO $ WS.sendTextData conn $ encode ((T.pack "patch", patchList), clock)
