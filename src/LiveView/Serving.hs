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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Serving where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM (atomically)
import Control.Lens
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL
import Data.Hashable
import Data.HashMap.Strict hiding ((!?))
import qualified Data.HashMap.Strict as HM
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Import
import LiveView.Html
import Lucid qualified as L
import Streaming
import qualified Streaming.Prelude as S


newtype Clock = Clock {
  _unClock :: Int 
} deriving (Eq, Hashable, Enum, FromJSON, Ord, Show, ToJSON)


data ActionCall = ActionCall
  { _action :: T.Text,
    _payload :: HashMap T.Text T.Text
  } deriving Show

deriveFromJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''ActionCall

mergePar :: Stream (Of a) IO () -> Stream (Of a) IO () -> Stream (Of a) IO ()
mergePar a b = do
    chan <- liftIO STM.newTChanIO
    let writeThread stream = S.mapM_ (atomically . STM.writeTChan chan . Just)
                                stream >> atomically (STM.writeTChan chan Nothing)
    a' <- liftIO $ Async.async (writeThread a)
    b' <- liftIO $ Async.async (writeThread b)
    liftIO $ Async.link a'
    liftIO $ Async.link b'
    let go = do
          a'' <- liftIO $ Async.poll a'
          b'' <- liftIO $ Async.poll b'
          case (a'', b'') of
            (Just _, _) -> liftIO $ void $ Async.waitEither a' b'
            (_, Just _) -> liftIO $ void $ Async.waitEither a' b'
            (Nothing, Nothing) -> do
              el <- liftIO $ atomically $ STM.readTChan chan
              case el of
                Nothing -> pure ()
                Just x -> S.yield x
              go
    go

data DepInput = DepHtml (L.Html ()) | DepMessage BL.ByteString

data LiveViewDeps r m = LiveViewDeps
  { _lvdInitialHtml :: L.Html ()
  , _lvdInputStream :: Stream (Of DepInput) m ()
  , _lvdSendSocketMessage :: BL.ByteString -> m ()
  , _lvdDebugPrint :: String -> m ()
  }

commuteState :: (Monad m) => s -> Stream (Of a) (StateT s m) r -> Stream (Of a) m (r, s)
commuteState s stream =
  runStateT (distribute stream) s

serveLV :: forall r m. (Monad m) => LiveViewDeps r m -> Stream (Of ActionCall) m ()
serveLV deps = do
  let initHtml = _lvdInitialHtml deps
      mountList = toSplitText initHtml
  lift $ _lvdDebugPrint deps "start"
  lift $ _lvdSendSocketMessage deps $ encode (("mount" :: T.Text, mountList), Clock 0)
  let asStatefulStream :: Stream (Of DepInput) (StateT (L.Html (), Clock) m) ()
      asStatefulStream = hoist lift $ _lvdInputStream deps
      actionCallStatefulStream :: Stream (Of ActionCall) (StateT (L.Html (), Clock) m) ()
      actionCallStatefulStream = S.for asStatefulStream $ \case
        DepHtml h -> do
          lift $ lift $ _lvdDebugPrint deps "DepState"
          prevHtml <- _1 <<.= h
          clock <- _2 <%= succ
          lift $ lift $ _lvdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml prevHtml h), clock)
        DepMessage rawMessage -> do
          lift $ lift $ _lvdDebugPrint deps "DepMessage"
          case (decode rawMessage :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, clock) -> do
              lift $ lift $ _lvdDebugPrint deps "yield call"
              S.yield call
  void $ commuteState (initHtml, Clock 0) actionCallStatefulStream
