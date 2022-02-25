{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LiveView where

import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.Async qualified as Async
import Control.Lens
import Control.Lens qualified as L
import Control.Monad.Base
import Control.Monad.Except
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
import Focus qualified
import Import
import ListT qualified
import LiveView.Html
import LiveView.Serving
import Lucid (Html)
import Lucid qualified as L
import Lucid.Base (commuteHtmlT)
import Lucid.Base qualified as L
import StmContainers.Map qualified as StmMap
import Streaming
import Streaming.Prelude qualified as S
import System.Random (StdGen)
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

newtype StateStoreError token = NoSuchTopic token
  deriving (Show, Eq)

data StateStore m token mutator state = StateStore
  { _subscribeState :: token -> m (state, Stream (Of state) m ()),
    _mutateState :: token -> mutator -> m (),
    _deleteState :: token -> m (),
    _existsState :: token -> m Bool,
    _allStates :: m (Stream (Of [(token, state)]) m ())
  }

mapAllStatesStream :: (Monad m) => (a -> b) -> m (Stream (Of [(token, a)]) m ()) -> m (Stream (Of [(token, b)]) m ())
mapAllStatesStream f list = S.map (fmap (\(t,s) -> (t, f s))) <$> list

makeClassy ''StateStore

instance (Monad m) => Profunctor (StateStore m token) where
  rmap f (StateStore sub mut del exists list) =
    StateStore
      ( \tok -> do
          (x, xs) <- sub tok
          let x' = f x
              xs' = S.map f xs
          pure (x', xs')
      )
      mut
      del
      exists
      (mapAllStatesStream f list)
  lmap f (StateStore sub mut del exists list) =
    StateStore
      sub
      (\tok mutr -> mut tok (f mutr))
      del
      exists
      list

hoistM :: (Monad m, Monad n) => (forall a. m a -> n a) -> StateStore m token mutator state -> StateStore n token mutator state
hoistM f (StateStore sub mut del ext list) = StateStore
  (\tok -> do
      (h,tl) <- f $ sub tok
      pure (h, hoist f tl))
  (f .: mut)
  (f . del)
  (f . ext)
  (hoist f <$> f list)

inMemoryStateStore ::
  forall state k m.
  (Eq k, Ord k, Hashable k, Monad m) =>
  -- consider making this a MonadBase STM constraint instead.
  (forall a. STM a -> m a) ->
  m state ->
  m (StateStore m k (state -> WithAction m state) state)
inMemoryStateStore liftSTM mkState = do
  stmMap :: StmMap.Map k (state, STM.TChan (Either () state)) <- liftSTM StmMap.new
  let mkStateWithChan :: m (state, STM.TChan (Either () state))
      mkStateWithChan = (,) <$> mkState <*> liftSTM STM.newBroadcastTChan
      getOrInit :: k -> m (state, STM.TChan (Either () state))
      getOrInit k = do
        mayV <- liftSTM $ StmMap.lookup k stmMap
        case mayV of
          Nothing -> do
            v <- mkStateWithChan
            liftSTM $ StmMap.insert v k stmMap >> pure v
          Just v -> pure v
      subscribe :: k -> m (state, Stream (Of state) m ())
      subscribe token = do
        (s, wChan) <- getOrInit token
        rChan <- liftSTM $ STM.dupTChan wChan
        pure (s, S.untilLeft (liftSTM $ STM.readTChan rChan))
      mutate ::
        forall a.
        IntoWithAction m a state =>
        k ->
        (state -> a) ->
        m ()
      mutate token f = do
        (s, wChan) <- getOrInit token
        let (s', w) = runWriter $ runWithAction $ intoWithAction (f s)
        liftSTM $ do
          STM.writeTChan wChan $ Right s'
          StmMap.insert (s', wChan) token stmMap
        getAction w
      delete token = do
        mayPair <- liftSTM $ StmMap.lookup token stmMap
        case mayPair of
          Nothing -> pure ()
          Just (s, wChan) -> liftSTM $ do
            STM.writeTChan wChan $ Left ()
            StmMap.delete token stmMap
      exists token = do
        liftSTM $ StmMap.lookup token stmMap <&> isJust
      list = do
        assocs <- liftSTM $ ListT.toList $ StmMap.listT stmMap
        pure $ S.yield (fmap (second fst) assocs)
  pure $ StateStore subscribe mutate delete exists list

newtype BindingKey = BindingKey {_unBindingKey :: Int}

newtype Renderer state mutator m a = Renderer
  { runRenderer ::
      ReaderT
        state
        (StateT BindingState (WriterT (BindingMap mutator) m))
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader state,
      MonadState BindingState,
      MonadWriter (BindingMap mutator)
    )

instance MonadTrans (Renderer state mutator) where
  lift :: (Monad m) => m a -> Renderer state mutator m a
  lift ma = Renderer $ lift $ lift $ lift ma

instance MFunctor (Renderer state mutator) where
  hoist :: Monad m => (forall a. m a -> n a) -> Renderer state mutator m b -> Renderer state mutator n b
  hoist f (Renderer readStateWriterMa) = Renderer $ hoist (hoist (hoist f)) readStateWriterMa

newtype WrappedRenderer m a state mutator = WrappedRenderer
  { unwrapRenderer :: Renderer state mutator m a
  }

instance (Monad m) => Profunctor (WrappedRenderer m a) where
  rmap f (WrappedRenderer (Renderer readT)) = WrappedRenderer $
    Renderer $ hoist (hoist (mapWriterT (\mpair -> do
      (a, bindMap) <- mpair
      pure (a, f <$> bindMap)
      ))) readT
  lmap f (WrappedRenderer (Renderer readT)) = WrappedRenderer $ Renderer $
    withReaderT f readT

type LiveViewT state mutator m a = L.HtmlT (Renderer state mutator m) a
type LiveView state mutator = LiveViewT state mutator STM ()

newtype WrappedLiveView m state mutator = WrappedLiveView
  { unwrapLiveView :: LiveViewT state mutator m ()
  }

instance (Monad m) => Profunctor (WrappedLiveView m) where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> WrappedLiveView m b c -> WrappedLiveView m a d
  dimap f g (WrappedLiveView x) =
    WrappedLiveView $ hoist morphRend x
    where
      morphRend :: Renderer b c m e -> Renderer a d m e
      morphRend rend =
            let wrappedRend = WrappedRenderer rend
            in unwrapRenderer $ dimap f g wrappedRend


dimapLiveView :: Monad m => (state -> b)
     -> (c -> mutator) -> LiveViewT b c m () -> LiveViewT state mutator m ()
dimapLiveView f g = unwrapLiveView . dimap f g . WrappedLiveView

zoomLiveView :: Monad m => Lens' b a -> LiveViewT a (a -> a) m () -> LiveViewT b (b -> b) m ()
zoomLiveView l = dimapLiveView (^. l) (l %~)

fromSettingMutator :: Monad m => LiveViewT a b m () -> LiveViewT a (c -> b) m ()
fromSettingMutator = dimapLiveView id const

toSettingMutator :: Monad m => LiveViewT a (a -> b) m () -> LiveViewT a b m ()
toSettingMutator lv = do
  a <- ask
  dimapLiveView id ($ a) lv

addActionBinding ::
  (Monad m) =>
  T.Text ->
  (BindingCall -> mutator) ->
  LiveViewT state mutator m Hsaction
addActionBinding event callback = do
  seed <- bindingIdSeed <<%= (+ 1)
  tell $ BindingMap $ MonoidalIntMap.singleton seed [callback]
  pure $ makeHsaction event (tshow seed)

data ServDeps = ServDeps
  { _sdSendSocketMessage :: BL.ByteString -> IO (),
    _sdDebugPrint :: String -> IO ()
  }

runLiveView :: forall m state mutator a.
  (Functor m) =>
  LiveViewT state mutator m a ->
  state ->
  BindingState ->
  m ((Html a, BindingState), BindingMap mutator)
runLiveView lv s bs =
  let rendOfHtml :: Renderer state mutator m (Html a)
      rendOfHtml = L.commuteHtmlT lv
      ranRend :: ReaderT state (StateT BindingState (WriterT (BindingMap mutator) m)) (Html a)
      ranRend = runRenderer rendOfHtml
      stateT' :: StateT BindingState (WriterT (BindingMap mutator) m) (Html a)
      stateT' = runReaderT ranRend s
      writerT' :: WriterT (BindingMap mutator) m (Html a, BindingState)
      writerT' = runStateT stateT' bs
  in runWriterT writerT'


serveLiveView :: forall token mutator state.
  ServDeps ->
  StateStore IO token mutator state ->
  LiveView state mutator ->
  Stream (Of BL.ByteString) IO () ->
  token ->
  (IO (Html ()), Stream (Of BL.ByteString) IO ())
serveLiveView deps store lv incomingMessages token =
  (init, live)
  where
    init = do
      (initialState, _) <- _subscribeState store token
      ((initHtml, _), _) <- atomically $ runLiveView lv initialState nullBindingState
      pure initHtml
    live :: Stream (Of BL.ByteString) IO ()
    live = do
      (initialState, states, initHtml, initBs, bindMap) <- liftIO $ do
        (initialState, states) <- _subscribeState store token
        ((initHtml, initBs), bindMap) <- atomically $ runLiveView lv initialState nullBindingState
        pure (initialState, states, initHtml, initBs, bindMap)

      let initClock = Clock 0
      (ioref :: IORef (Html (), BindingMap mutator, Clock, state))
        <- liftIO $ newIORef (initHtml, bindMap, initClock, initialState)
      liftIO $ _sdSendSocketMessage deps $
        encode (("mount" :: T.Text, toSplitText initHtml), initClock)
      let inpStream = mergePar (S.map Left states) (S.map Right incomingMessages)
      flip S.mapM_ (hoist liftIO inpStream) $ \case
        Left s -> do
          (h, _, c, _) <- liftIO $ readIORef ioref
          let c' = succ c
          ((h', _), bm') <- liftIO $ atomically $ runLiveView lv s nullBindingState
          liftIO $ _sdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml h h'), c')
          liftIO $ writeIORef ioref (h', bm', c', s)
        Right msg -> do
          case (decode msg :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, callClock) -> do
              (_, bm, clock, s) <- liftIO $ readIORef ioref
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
                    Just handlers -> forM_ handlers (liftIO . _mutateState store token . ($ BindingCall (_payload call ^? ix "value")))
                else liftIO $ _sdDebugPrint deps $ "No matching handler for " <> show msg
