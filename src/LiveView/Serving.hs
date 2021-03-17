{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Serving where

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

deriveFromJSON (defaultOptions { fieldLabelModifier = drop 1}) ''ActionCall

data DepInput r = DepState r | DepMessage BL.ByteString

data LiveViewDeps r m = LiveViewDeps
  { _lvdInitialState :: r
  , _lvdInputStream :: Stream (Of (DepInput r)) m ()
  , _lvdLiveView :: LiveView r ()
  , _lvdSendSocketMessage :: BL.ByteString -> m ()
  , _lvdRootWrapper :: L.Html () -> L.Html ()
  , _lvdDebugPrint :: String -> m ()
  }

ofFst :: Of a b -> a
ofFst (a Streaming.:> b) = a

type StatefulStreamPair m = ((L.Html (), Clock), Stream (Of ActionCall) (StateT (L.Html (), Clock) m) ())

resolveState :: (Monad m) => s -> Stream (Of a) (StateT s m) r -> Stream (Of a) m (r, s)
resolveState s stream = S.unfoldr unfoldFn (s, stream)
  where unfoldFn (currState, stream) = do
          (eithNextResult, nextState) <- runStateT (S.next stream) currState
          case eithNextResult of
            Left r -> pure $ Left (r, nextState)
            Right (a, stream') -> pure $ Right (a, (nextState, stream'))

serveLV :: forall r m. (Monad m) => LiveViewDeps r m -> Stream (Of ActionCall) m ()
serveLV deps = do
  let getHtml r = _lvdRootWrapper deps $
        _html $ getLiveViewResult r (_lvdLiveView deps)
      initHtml = getHtml (_lvdInitialState deps)
      mountList = toSplitText initHtml
  lift $ _lvdDebugPrint deps "start"
  lift $ _lvdSendSocketMessage deps $ encode (("mount" :: T.Text, mountList), Clock 0)
  let asStatefulStream :: Stream (Of (DepInput r)) (StateT (L.Html (), Clock) m) ()
      asStatefulStream = hoist lift $ _lvdInputStream deps
      actionCallStatefulStream :: Stream (Of ActionCall) (StateT (L.Html (), Clock) m) ()
      actionCallStatefulStream = S.for asStatefulStream $ \case
        DepState r -> do
          lift $ lift $ _lvdDebugPrint deps "DepState"
          let nowHtml = getHtml r
          prevHtml <- _1 <<.= nowHtml
          clock <- _2 <%= succ
          lift $ lift $ _lvdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml prevHtml nowHtml), clock)
        DepMessage rawMessage -> do
          lift $ lift $ _lvdDebugPrint deps "DepMessage"
          case (decode rawMessage :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, clock) -> do
              lift $ lift $ _lvdDebugPrint deps "yield call"
              S.yield call
  void $ resolveState (initHtml, Clock 0) actionCallStatefulStream
