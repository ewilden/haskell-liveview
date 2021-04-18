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
import Streamly qualified as SY
import Streamly.Prelude qualified as SY


newtype Clock = Clock {
  _unClock :: Int 
} deriving (Eq, Hashable, Enum, FromJSON, Ord, Show, ToJSON)


data ActionCall = ActionCall
  { _action :: T.Text,
    _payload :: HashMap T.Text T.Text
  } deriving Show

deriveFromJSON (defaultOptions { fieldLabelModifier = drop 1}) ''ActionCall

fromStreaming :: (SY.IsStream t, SY.MonadAsync m) => Stream (Of a) m r -> t m a
fromStreaming = SY.unfoldrM S.uncons

toStreaming :: Monad m => SY.SerialT m a -> Stream (Of a) m ()
toStreaming = S.unfoldr unconsS
    where
    -- Adapt S.uncons to return an Either instead of Maybe
    unconsS s = SY.uncons s >>= maybe (return $ Left ()) (return . Right)

mergePar :: Stream (Of a) IO () -> Stream (Of a) IO () -> (Stream (Of a) IO ())
mergePar a b =
  -- toStreaming $ SY.parallely $ (fromStreaming a) <> (fromStreaming b)
  do
    chan <- liftIO STM.newTChanIO
    let writeThread stream = S.mapM_ (atomically . STM.writeTChan chan . Just) stream >> (atomically (STM.writeTChan chan Nothing))
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
commuteState s stream = S.unfoldr unfoldFn (s, stream)
  where unfoldFn (currState, stream) = do
          (eithNextResult, nextState) <- runStateT (S.next stream) currState
          case eithNextResult of
            Left r -> pure $ Left (r, nextState)
            Right (a, stream') -> pure $ Right (a, (nextState, stream'))

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
