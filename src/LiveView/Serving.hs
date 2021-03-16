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
  { _initialState :: r
  , _inputStream :: Stream (Of (DepInput r)) m ()
  , _liveView :: LiveView r ()
  , _sendSocketMessage :: BL.ByteString -> m ()
  , _sendActionCall :: ActionCall -> m ()
  , _debugPrint :: String -> m ()
  }

serveLV :: (Monad m) => LiveViewDeps r m -> m ()
serveLV deps = do
  let getHtml r = _html $ getLiveViewResult r (_liveView deps)
      initHtml = getHtml (_initialState deps)
      mountList = toSplitText initHtml
  _debugPrint deps "start"
  _sendSocketMessage deps $ encode (("mount" :: T.Text, mountList), Clock 0)
  flip evalStateT (initHtml, Clock 0) 
    $ flip S.mapM_ (hoist lift $ _inputStream deps) $ \case 
        DepState r -> do
          lift $ _debugPrint deps "DepState"
          let nowHtml = getHtml r
          prevHtml <- _1 <<.= nowHtml
          clock <- _2 <%= succ
          lift $ _sendSocketMessage deps $ encode (("patch" :: T.Text, diffHtml prevHtml nowHtml), clock)
        DepMessage rawMessage -> do
          lift $ _debugPrint deps "DepMessage"
          case (decode rawMessage :: Maybe (ActionCall, Clock)) of
            Nothing -> pure ()
            Just (call, clock) -> lift $ _sendActionCall deps call
