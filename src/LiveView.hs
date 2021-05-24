{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module LiveView where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict (MonadWriter (tell), Writer, WriterT (runWriterT, WriterT), runWriter, writer, mapWriterT)
import Data.Aeson hiding ((.:), (.=))
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL
import Data.Composition ((.:))
import Data.HashMap.Monoidal (MonoidalHashMap)
import Data.HashMap.Strict hiding (foldr, (!?))
import Data.HashMap.Strict qualified as HM hiding (foldr)
import Data.Hashable
import Data.IORef
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.IntMap.Monoidal.Strict qualified as MonoidalIntMap
import Data.Profunctor
import Data.Semigroup.Monad
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Import
import LiveView.Html
import LiveView.Serving
import Lucid (Html)
import Lucid qualified as L
import Lucid.Base (commuteHtmlT)
import Lucid.Base qualified as L
import StmContainers.Map qualified as StmMap
import Streaming
import Streaming.Prelude qualified as S
import Text.Read (readMaybe)

newtype WithAction m a = WithAction
  { runWithAction :: Writer (Action m) a
  }
  deriving (Functor, Applicative, Monad, MonadWriter (Action m))

class IntoWithAction m a b where
  intoWithAction :: a -> WithAction m b

instance (Monad m) => IntoWithAction m a a where
  intoWithAction = pure

instance (Monad m) => IntoWithAction m (WithAction m a) a where
  intoWithAction = id

instance (Monad m) => IntoWithAction m (m (), a) a where
  intoWithAction (m, a) = writer (a, Action m)

newtype RenderState state = RenderState
  { _currState :: state
  }

newtype BindingCall = BindingCall
  { _value :: Maybe T.Text
  }

newtype BindingState = BindingState
  { _bindingIdSeed :: Int
  }

makeClassy ''BindingState

newtype BindingMap mutator = BindingMap
  { _actionBindings :: MonoidalIntMap [BindingCall -> mutator]
  }
  deriving (Semigroup, Monoid, Functor)

makeClassy ''BindingMap

nullBindingState :: BindingState
nullBindingState = BindingState 1

data StateStore token mutator state = StateStore
  { _subscribeState :: token -> IO (state, Stream (Of state) IO ()),
    _mutateState :: token -> mutator -> IO (),
    _deleteState :: token -> IO ()
  }

makeClassy ''StateStore

instance Profunctor (StateStore token) where
  rmap f (StateStore sub mut del) =
    StateStore
      ( \tok -> do
          (x, xs) <- sub tok
          let x' = f x
              xs' = S.map f xs
          pure (x', xs')
      )
      mut
      del
  lmap f (StateStore sub mut del) =
    StateStore
      sub
      (\tok mutr -> mut tok (f mutr))
      del

getOrInit :: (Eq k, Ord k, Hashable k) => STM.STM a -> k -> StmMap.Map k a -> STM.STM a
getOrInit def k m = do
  mayV <- StmMap.lookup k m
  case mayV of
    Nothing -> do
      v <- def
      StmMap.insert v k m >> pure v
    Just v -> pure v

getOrInitM ::
  (MonadIO m, Eq k, Ord k, Hashable k) =>
  m a ->
  k ->
  StmMap.Map k a ->
  m a
getOrInitM mdef k m = do
  def <- mdef
  liftIO $ STM.atomically $ getOrInit (pure def) k m

inMemoryStateStore ::
  forall state k.
  (Eq k, Ord k, Hashable k) =>
  IO state ->
  IO (StateStore k (state -> WithAction IO state) state)
inMemoryStateStore mkState = do
  stmMap :: StmMap.Map k (state, STM.TChan (Either () state)) <- StmMap.newIO
  let mkStateWithChan = (,) <$> mkState <*> STM.newBroadcastTChanIO
      subscribe :: k -> IO (state, Stream (Of state) IO ())
      subscribe token = do
        (s, wChan) <- getOrInitM mkStateWithChan token stmMap
        rChan <- atomically $ STM.dupTChan wChan
        pure (s, S.untilLeft $ STM.atomically (STM.readTChan rChan))
      mutate ::
        forall a.
        IntoWithAction IO a state =>
        k ->
        (state -> a) ->
        IO ()
      mutate token f = do
        mayPair <- atomically $ StmMap.lookup token stmMap
        case mayPair of
          Nothing -> pure ()
          Just (s, wChan) -> do
            let (s', w) = runWriter $ runWithAction $ intoWithAction (f s)
            atomically $ do
              STM.writeTChan wChan $ Right s'
              StmMap.insert (s', wChan) token stmMap
            getAction w
      delete token = atomically $ do
        mayPair <- StmMap.lookup token stmMap
        case mayPair of
          Nothing -> pure ()
          Just (s, wChan) -> do
            STM.writeTChan wChan $ Left ()
            StmMap.delete token stmMap
  pure $ StateStore subscribe mutate delete

newtype BindingKey = BindingKey {_unBindingKey :: Int}

newtype Renderer state mutator a = Renderer
  { runRenderer ::
      ReaderT
        state
        (StateT BindingState (WriterT (BindingMap mutator) IO))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader state,
      MonadState BindingState,
      MonadWriter (BindingMap mutator),
      MonadIO
    )

newtype WrappedRenderer a state mutator = WrappedRenderer
  { unwrapRenderer :: Renderer state mutator a
  }

instance Profunctor (WrappedRenderer a) where
  rmap f (WrappedRenderer (Renderer readT)) = WrappedRenderer $
    Renderer $ hoist (hoist $ mapWriterT mapper) readT
      where mapper x = fmap (fmap (fmap f)) x
  lmap f (WrappedRenderer (Renderer readT)) = WrappedRenderer $ Renderer $
    withReaderT f readT

class (Monad m) => MonadRenderer state mutator m | m -> state, m -> mutator where
  liftRenderer :: Renderer state mutator a -> m a

instance MonadRenderer state mutator (Renderer state mutator) where
  liftRenderer = id

instance
  (Monad (t m), MonadRenderer state mutator m, MonadTrans t) =>
  MonadRenderer state mutator (t m)
  where
  liftRenderer = lift . liftRenderer

type LiveView state mutator = L.HtmlT (Renderer state mutator) ()

newtype WrappedLiveView state mutator = WrappedLiveView
  { unwrapLiveView :: LiveView state mutator
  }

instance Profunctor WrappedLiveView where
  dimap f g (WrappedLiveView x) = WrappedLiveView $ hoist dimap' x
    where dimap' = unwrapRenderer . dimap f g . WrappedRenderer


addActionBinding ::
  (MonadRenderer s mutator m) =>
  T.Text ->
  (BindingCall -> mutator) ->
  m Hsaction
addActionBinding event callback = liftRenderer $ do
  seed <- bindingIdSeed <<%= (+ 1)
  tell $ BindingMap $ MonoidalIntMap.singleton seed [callback]
  pure $ makeHsaction event (tshow seed)

data ServDeps = ServDeps
  { _sdSendSocketMessage :: BL.ByteString -> IO (),
    _sdDebugPrint :: String -> IO ()
  }

runLiveView :: forall state mutator.
  LiveView state mutator ->
  state ->
  BindingState ->
  IO ((Html (), BindingState), BindingMap mutator)
runLiveView lv s bs =
  let rendOfHtml :: Renderer state mutator (Html ())
      rendOfHtml = L.commuteHtmlT lv
      ranRend :: ReaderT state (StateT BindingState (WriterT (BindingMap mutator) IO)) (Html ())
      ranRend = runRenderer rendOfHtml
      stateT' :: StateT BindingState (WriterT (BindingMap mutator) IO) (Html ())
      stateT' = runReaderT ranRend s
      writerT' :: WriterT (BindingMap mutator) IO (Html (), BindingState)
      writerT' = runStateT stateT' bs
  in runWriterT writerT'
  -- L.commuteHtmlT lv
    -- & runRenderer
    -- & flip runReaderT s
    -- & flip runStateT bs
    -- & flip runWriterT


serveLiveView :: forall token mutator state.
  ServDeps ->
  StateStore token mutator state ->
  LiveView state mutator ->
  Stream (Of BL.ByteString) IO () ->
  token ->
  (IO (Html ()), IO ())
serveLiveView deps store lv incomingMessages token =
  (init, live)
  where
    init = do
      (initialState, _) <- _subscribeState store token
      ((initHtml, _), _) <- runLiveView lv initialState nullBindingState
      pure initHtml
    live = do
      (initialState, states) <- _subscribeState store token
      ((initHtml, initBs), bindMap) <- runLiveView lv initialState nullBindingState
      let initClock = Clock 0
      (ioref :: IORef (Html (), BindingMap mutator, Clock, state))
        <- newIORef (initHtml, bindMap, initClock, initialState)
      _sdSendSocketMessage deps $
        encode (("mount" :: T.Text, toSplitText initHtml), initClock)
      let inpStream = mergePar (S.map Left states) (S.map Right incomingMessages)
      flip S.mapM_ inpStream $ \case
        Left s -> do
          (h, _, c, _) <- readIORef ioref
          let c' = succ c
          ((h', _), bm') <- runLiveView lv s nullBindingState
          _sdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml h h'), c')
          writeIORef ioref (h', bm', c', s)
        Right msg -> do
          case (decode msg :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, callClock) -> do
              (_, bm, clock, s) <- readIORef ioref
              if callClock == clock
                then do
                  let mayBSeed = readMaybe @Int (T.unpack $ _action call)
                      mayHandler = do
                        seed <- mayBSeed
                        bm
                          ^? actionBindings
                            . to MonoidalIntMap.getMonoidalIntMap
                            . ix seed
                  case mayHandler of
                    Nothing -> pure ()
                    Just handlers -> forM_ handlers (_mutateState store token . ($ BindingCall (_payload call ^? ix "value")))
                else _sdDebugPrint deps $ "No matching handler for " <> show msg
