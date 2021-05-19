{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module LiveView where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict (WriterT)
import Data.Aeson hiding ((.=))
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Monoidal (MonoidalHashMap)
import Data.HashMap.Strict hiding ((!?))
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.IORef
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.IntMap.Monoidal.Strict qualified as MonoidalIntMap
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Import
import LiveView.Html
import LiveView.Serving
import Lucid (Html)
import Lucid qualified as L
import Lucid.Base qualified as L
import Streaming
import Streaming.Prelude qualified as S

newtype RenderState state = RenderState
  { _currState :: state
  }

newtype BindingCall = BindingCall
  { _value :: Maybe T.Text
  }

data BindingState state = BindingState
  { _actionBindings :: MonoidalIntMap [BindingCall -> state -> IO state],
    _bindingIdSeed :: Int
  }

makeClassy ''BindingState

nullBindingState :: BindingState s
nullBindingState = BindingState mempty 1

data StateStore token state = StateStore
  { _subscribeState :: token -> IO (state, Stream (Of state) IO ()),
    _mutateState :: token -> (state -> IO state) -> IO ()
  }

makeClassy ''StateStore

newtype BindingKey = BindingKey {_unBindingKey :: Int}

newtype Renderer state a = Renderer
  { runRenderer ::
      ReaderT
        state
        (StateT (BindingState state) IO)
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader state,
      MonadState (BindingState state),
      MonadIO
    )

class (Monad m) => MonadRenderer state m | m -> state where
  liftRenderer :: Renderer state a -> m a

instance MonadRenderer state (Renderer state) where
  liftRenderer = id

instance
  (Monad (t m), MonadRenderer state m, MonadTrans t) =>
  MonadRenderer state (t m)
  where
  liftRenderer = lift . liftRenderer

type LiveView state = L.HtmlT (Renderer state) ()

addActionBinding ::
  (MonadRenderer s m) =>
  T.Text ->
  (BindingCall -> s -> IO s) ->
  m Hsaction
addActionBinding event callback = liftRenderer $ do
  seed <- bindingIdSeed <<%= (+ 1)
  actionBindings %= (<> MonoidalIntMap.singleton seed [callback])
  pure $ makeHsaction event (tshow seed)

data ServDeps = ServDeps
  { _sdSendSocketMessage :: BL.ByteString -> IO (),
    _sdDebugPrint :: String -> IO ()
  }

runLiveView ::
  LiveView state ->
  state ->
  BindingState state ->
  IO (Html (), BindingState state)
runLiveView lv s bs =
  L.commuteHtmlT lv
    & runRenderer
    & flip runReaderT s
    & flip runStateT bs

serveLiveView ::
  ServDeps ->
  StateStore token state ->
  LiveView state ->
  Stream (Of BL.ByteString) IO () ->
  token ->
  IO ()
serveLiveView deps store lv incomingMessages token = do
  (initialState, states) <- _subscribeState store token
  (initHtml, initBs) <- runLiveView lv initialState nullBindingState
  let initClock = Clock 0
  ioref <- newIORef (initHtml, initBs, initClock)
  _sdSendSocketMessage deps $
    encode (("mount" :: T.Text, toSplitText initHtml), initClock)
  let inpStream = mergePar (S.map Left states) (S.map Right incomingMessages)
  flip S.mapM_ inpStream $ \case
    Left s -> do
      (h, _, c) <- readIORef ioref
      let c' = succ c
      (h', bs') <- runLiveView lv s nullBindingState
      _sdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml h h'), c')
      writeIORef ioref (h', bs', c')
    Right msg -> do
      case (decode msg :: Maybe (ActionCall, Clock)) of
        Nothing -> pure ()
        Just (call, clock) -> do
          (_, bs, c) <- readIORef ioref
          -- TODO: actually use ActionCall and bound callback (incl value)
          undefined
