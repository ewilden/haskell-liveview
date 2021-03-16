{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (atomically)
import Control.Lens hiding ((.=))
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
import Network.Wai.Handler.Warp qualified as Warp
import Servant hiding (Stream)
import Streaming
import Streaming.Prelude qualified as S

type API = LiveViewApi :<|> Raw

api :: Proxy API
api = Proxy

data Op = Add | Subtract | Multiply | Divide deriving (Eq, Show, Read)

sampleLiveView :: LiveView (Float, Op, Float) ()
sampleLiveView = div_ [id_ "lvroot"] $ do
  (x, op, y) <- ask
  onChangeX <- makeHsaction "change" "change_x"
  input_ [value_ $ tshow x, hsaction_ onChangeX]
  onChangeOp <- makeHsaction "change" "change_op"
  select_ [hsaction_ onChangeOp] $ forM_ [Add, Subtract, Multiply, Divide] $ \op' ->
    option_ ([value_ $ tshow op'] ++ if op == op' then [selected_ "true"] else []) 
      $ fromString $ show op'
  onChangeY <- makeHsaction "change" "change_y"
  input_ [value_ $ tshow y, hsaction_ onChangeY]
  " = "
  let opFn = case op of
        Add -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Divide -> (/)
  input_ [value_ $ tshow (opFn x y)]
  script_ [src_ "index.js"] $ T.pack ""

reducer :: ActionCall -> State (Float, Op, Float) ()
reducer (ActionCall action payload) =
  if action == "change_x" then
    _1 .= (read $ T.unpack $ payload ^?! ix "value")
  else if action == "change_y" then
    _3 .= (read $ T.unpack $ payload ^?! ix "value")
  else if action == "change_op" then
    _2 .= (read $ T.unpack $ payload ^?! ix "value")
  else pure ()

server :: Server API
server = serveLVServant (do
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
            pure $ ServantLVDeps initS stateStream sampleLiveView actionCallback
          ) :<|> serveDirectoryWebApp "static"

main :: IO ()
main = Warp.run 5000 (serve api server)
