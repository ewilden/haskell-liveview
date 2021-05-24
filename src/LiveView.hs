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
import Control.Monad.Writer.Strict (MonadWriter, Writer, WriterT, writer, runWriter)
import Data.Aeson hiding ((.=), (.:))
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Monoidal (MonoidalHashMap)
import Data.HashMap.Strict hiding (foldr, (!?))
import Data.HashMap.Strict qualified as HM hiding (foldr)
import Data.Hashable
import Data.IORef
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.IntMap.Monoidal.Strict qualified as MonoidalIntMap
import Data.Semigroup.Monad
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Import
import LiveView.Html
import LiveView.Serving
import Lucid (Html)
import Lucid qualified as L
import Lucid.Base qualified as L
import StmContainers.Map qualified as StmMap
import Streaming
import Streaming.Prelude qualified as S
import Text.Read (readMaybe)
import Data.Composition ((.:))

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

data BindingState state mutator = BindingState
  { _actionBindings :: MonoidalIntMap [BindingCall -> mutator],
    _bindingIdSeed :: Int
  }

makeClassy ''BindingState

nullBindingState :: BindingState s a
nullBindingState = BindingState mempty 1


data StateStore token state mutator = StateStore
  { _subscribeState :: token -> IO (state, Stream (Of state) IO ()),
    _mutateState :: token -> mutator -> IO (),
    _deleteState :: token -> IO ()
  }

makeClassy ''StateStore

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
  IO (StateStore k state (state -> WithAction IO state))
inMemoryStateStore mkState = do
  stmMap :: StmMap.Map k (state, STM.TChan (Either () state)) <- StmMap.newIO
  let mkStateWithChan = (,) <$> mkState <*> STM.newBroadcastTChanIO
      subscribe :: k -> IO (state, Stream (Of state) IO ())
      subscribe token = do
        (s, wChan) <- getOrInitM mkStateWithChan token stmMap
        rChan <- atomically $ STM.dupTChan wChan
        pure (s, S.untilLeft $ STM.atomically (STM.readTChan rChan))
      mutate :: forall a. IntoWithAction IO a state =>
        k -> (state -> a) -> IO ()
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
        (StateT (BindingState state mutator) IO)
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader state,
      MonadState (BindingState state mutator),
      MonadIO
    )

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

addActionBinding ::
  (MonadRenderer s mutator m) =>
  T.Text ->
  (BindingCall -> mutator) ->
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
  LiveView state mutator ->
  state ->
  BindingState state mutator ->
  IO (Html (), BindingState state mutator)
runLiveView lv s bs =
  L.commuteHtmlT lv
    & runRenderer
    & flip runReaderT s
    & flip runStateT bs

serveLiveView ::
  ServDeps ->
  StateStore token state mutator ->
  LiveView state mutator ->
  Stream (Of BL.ByteString) IO () ->
  token ->
  (IO (Html ()), IO ())
serveLiveView deps store lv incomingMessages token =
  (init, live)
  where
    init = do
      (initialState, _) <- _subscribeState store token
      (initHtml, _) <- runLiveView lv initialState nullBindingState
      pure initHtml
    live = do
      (initialState, states) <- _subscribeState store token
      (initHtml, initBs) <- runLiveView lv initialState nullBindingState
      let initClock = Clock 0
      ioref <- newIORef (initHtml, initBs, initClock, initialState)
      _sdSendSocketMessage deps $
        encode (("mount" :: T.Text, toSplitText initHtml), initClock)
      let inpStream = mergePar (S.map Left states) (S.map Right incomingMessages)
      flip S.mapM_ inpStream $ \case
        Left s -> do
          (h, _, c, _) <- readIORef ioref
          let c' = succ c
          (h', bs') <- runLiveView lv s nullBindingState
          _sdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml h h'), c')
          writeIORef ioref (h', bs', c', s)
        Right msg -> do
          case (decode msg :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, callClock) -> do
              (_, bs, clock, s) <- readIORef ioref
              if callClock == clock
                then do
                  let mayBSeed = readMaybe @Int (T.unpack $ _action call)
                      mayHandler = do
                        seed <- mayBSeed
                        bs
                          ^? actionBindings
                            . to MonoidalIntMap.getMonoidalIntMap
                            . ix seed
                  case mayHandler of
                    Nothing -> pure ()
                    Just handlers -> forM_ handlers (_mutateState store token . ($ BindingCall (_payload call ^? ix "value")))
                else _sdDebugPrint deps $ "No matching handler for " <> show msg
