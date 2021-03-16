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
  , _lvdSendActionCall :: ActionCall -> m ()
  , _lvdRootWrapper :: L.Html () -> L.Html ()
  , _lvdDebugPrint :: String -> m ()
  }

serveLV :: (Monad m) => LiveViewDeps r m -> m ()
serveLV deps = do
  let getHtml r = _lvdRootWrapper deps $
        _html $ getLiveViewResult r (_lvdLiveView deps)
      initHtml = getHtml (_lvdInitialState deps)
      mountList = toSplitText initHtml
  _lvdDebugPrint deps "start"
  _lvdSendSocketMessage deps $ encode (("mount" :: T.Text, mountList), Clock 0)
  flip evalStateT (initHtml, Clock 0) 
    $ flip S.mapM_ (hoist lift $ _lvdInputStream deps) $ \case 
        DepState r -> do
          lift $ _lvdDebugPrint deps "DepState"
          let nowHtml = getHtml r
          prevHtml <- _1 <<.= nowHtml
          clock <- _2 <%= succ
          lift $ _lvdSendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml prevHtml nowHtml), clock)
        DepMessage rawMessage -> do
          lift $ _lvdDebugPrint deps "DepMessage"
          case (decode rawMessage :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, clock) -> lift $ _lvdSendActionCall deps call
