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

data ServantDeps = ServantDeps
  { _initialHtml :: Html ()
  , _htmls :: Stream (Of (Html ())) IO ()
  , _actionsCallback :: Stream (Of ActionCall) IO () -> IO ()
  , _basePage :: Maybe (Html () -> Html ())
  }

defaultBasePage :: Html () -> Html ()
defaultBasePage liveContent = do
   doctypehtml_ $ do
    head_ $ title_ "haskell liveview-simple"
    body_ liveContent
    script_ [src_ "index.js"] $ T.pack ""

serveLiveViewServant :: Handler ServantDeps -> Server LiveViewApi
serveLiveViewServant getDeps = initialRenderEndpoint :<|> liveRenderEndpoint
  where
    rootWrapper x = div_ [id_ "lvroot"] x
    initialRenderEndpoint = do
      (ServantDeps initHtml htmls actionCallback mayBasePage) <- getDeps
      liftIO $ putStrLn "initialRenderEndpoint"
      pure $ fromMaybe defaultBasePage mayBasePage (rootWrapper initHtml)
    liveRenderEndpoint conn = do
      (ServantDeps initHtml htmls actionsCallback mayBasePage) <- getDeps
      liftIO $ putStrLn "liveRenderEndpoint"
      inpChan <- liftIO STM.newTChanIO
      let writeHtmlsToInpChan = S.mapM_ (atomically . STM.writeTChan inpChan . DepHtml . rootWrapper) htmls
          writeMessagesToInpChan = forever $ do
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
