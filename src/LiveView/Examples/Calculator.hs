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
import LiveView
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

type AppState = (Float, Op, Float)

sampleLiveView :: LiveView AppState (AppState -> AppState)
sampleLiveView = do
  (x, op, y) <- ask
  let parseVal (BindingCall mayVal) = do
        rawVal <- mayVal
        readMaybe (T.unpack rawVal)
      addActionBindingForChange field = addActionBinding "change"
        (\bc -> field %~ (\f -> fromMaybe f (parseVal bc)))
  onChangeX <- addActionBindingForChange _1
  onChangeOp <- addActionBindingForChange _2
  onChangeY <- addActionBindingForChange _3

  input_ [value_ $ tshow x, hsaction_ onChangeX, type_ "number"]
  select_ [hsaction_ onChangeOp] $ forM_ [Add, Subtract, Multiply, Divide] $ \op' ->
    option_ ([value_ $ tshow op'] ++ if op == op' then [selected_ "true"] else []) 
      $ fromString $ show op'
  input_ [value_ $ tshow y, hsaction_ onChangeY, type_ "number"]
  " = "
  let opFn = case op of
        Add -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Divide -> (/)
  input_ [value_ $ tshow (opFn x y), type_ "number", readonly_ "true"]

type API = LiveViewApi :<|> Raw

api :: Proxy API
api = Proxy

server :: StateStore IO () (AppState -> AppState) AppState -> Server API
server store = (serveServantLiveView
               putStrLn
               (DefaultBasePage $ ScriptData
               { _liveViewScriptAbsolutePath = "/liveview.js"
               , _wssUrlSpec = Ws
               })
               "lvroot"
               store
               sampleLiveView
               ()) :<|> serveDirectoryWebApp "static"

initStateStore :: IO (StateStore IO () (AppState -> AppState) AppState)
initStateStore = lmap mapper <$> (inMemoryStateStore atomically (pure (1, Add, 1)))
  where mapper :: (AppState -> AppState) -> (AppState -> WithAction IO AppState)
        mapper = (intoWithAction . )

main :: IO ()
main = do
  store <- initStateStore
  Warp.run 5000 (serve api (server store))
