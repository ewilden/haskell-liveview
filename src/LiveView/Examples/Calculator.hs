{-# LANGUAGE OverloadedStrings #-}

module LiveView.Examples.Calculator where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (atomically)
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Reader
import Control.Monad.State
import Data.String (fromString)
import Data.Text qualified as T
import Import
import Lib
import LiveView.Html
import LiveView.Serving
import LiveView.Serving.Servant
import Lucid
import Lucid.Base (commuteHtmlT)
import Network.Wai.Handler.Warp qualified as Warp
import Servant hiding (Stream)
import Streaming
import Streaming.Prelude qualified as S
import Text.Read

data Op = Add | Subtract | Multiply | Divide deriving (Eq, Show, Read)

sampleLiveView :: HtmlT (Reader (Float, Op, Float)) ()
sampleLiveView = do
  (x, op, y) <- ask
  let onChangeX = makeHsaction "change" "change_x"
  input_ [value_ $ tshow x, hsaction_ onChangeX, type_ "number"]
  let onChangeOp = makeHsaction "change" "change_op"
  select_ [hsaction_ onChangeOp] $ forM_ [Add, Subtract, Multiply, Divide] $ \op' ->
    option_ ([value_ $ tshow op'] ++ if op == op' then [selected_ "true"] else []) 
      $ fromString $ show op'
  let onChangeY = makeHsaction "change" "change_y"
  input_ [value_ $ tshow y, hsaction_ onChangeY, type_ "number"]
  " = "
  let opFn = case op of
        Add -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Divide -> (/)
  input_ [value_ $ tshow (opFn x y), type_ "number", readonly_ "true"]

reducer :: ActionCall -> State (Float, Op, Float) ()
reducer (ActionCall action payload)
  | action == "change_x" = _1 %= \s -> fromMaybe s (payload ^? ix "value" <&> T.unpack >>= readMaybe)
  | action == "change_y" = _3 %= \s -> fromMaybe s (payload ^? ix "value" <&> T.unpack >>= readMaybe)
  | action == "change_op" = _2 %= \s -> fromMaybe s (payload ^? ix "value" <&> T.unpack >>= readMaybe)
  | otherwise = pure ()

type API = LiveViewApi :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveLiveViewServant (do
            let initS = (1, Add, 1)
            stateChan <- liftIO STM.newTChanIO
            currStateTV <- liftIO (STM.newTVarIO initS)
            let stateStream = S.repeatM (atomically $ STM.readTChan stateChan)
                actionCallback action = do
                  Prelude.putStrLn "actionCallback"
                  atomically $ do
                    currState <- STM.readTVar currStateTV
                    let nextState = execState (reducer action) currState
                    STM.writeTChan stateChan nextState
                    STM.writeTVar currStateTV nextState
                  Prelude.putStrLn "actionCallback 2"
                toHtml s = runReader (commuteHtmlT sampleLiveView) s
            pure $ ServantDeps 
              (toHtml initS) 
              (S.map toHtml stateStream) 
              (S.mapM_ actionCallback) 
              (DefaultBasePage $ ScriptData 
                { _liveViewScriptAbsolutePath = "/liveview.js"
                , _wssUrl = "ws://localhost:5000/liveview"
                })
              "lvroot"
          ) :<|> serveDirectoryWebApp "static"

main :: IO ()
main = Warp.run 5000 (serve api server)
