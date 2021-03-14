{-# LANGUAGE TemplateHaskell #-}

module LiveView.Serving where

import Control.Lens hiding ((.=))
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import Data.Aeson.TH
import Data.Hashable
import Data.HashMap.Strict hiding ((!?))
import qualified Data.HashMap.Strict as HM
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
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

data InputStreamEntry r 
  = InputState r
  | InputAction (ActionCall, Clock)
  deriving Show

isState :: InputStreamEntry r -> Bool
isState (InputState _) = True
isState _ = False

numTrueBefore :: Monad m => Stream (Of Bool) m r -> Stream (Of Int) m r
numTrueBefore = S.scan (\x a -> x + if a then 1 else 0) 0 id

mostRecentJustBefore :: Monad m => a -> Stream (Of (Maybe a)) m r -> Stream (Of a) m r
mostRecentJustBefore a = S.scan fromMaybe a id

data LiveViewInputs r m = LiveViewInputs
  { _initialState :: r,
    _inputStream :: Stream (Of (InputStreamEntry r)) m ()
  }

_stateStream :: (Monad m) => LiveViewInputs r m -> Stream (Of r) m ()
_stateStream inputs = _inputStream inputs 
  & S.mapMaybe (\case InputState r -> Just r; _ -> Nothing)

_actionStream :: (Monad m) => LiveViewInputs r m -> Stream (Of (ActionCall, Clock)) m ()
_actionStream inputs = _inputStream inputs 
  & S.mapMaybe (\case InputAction x -> Just x; _ -> Nothing)

data OutputStreamEntry
  = OutputAction ActionCall
  | OutputPatch ([PatchEntry T.Text], Clock)

data LiveViewOutputs m = LiveViewOutputs
  { _outputStream :: Stream (Of OutputStreamEntry) m (),
    _mountList :: ([T.Text], Clock),
    _firstRender :: L.Html ()
  }

{-
  Tricky thing to think about: JSON messages that come back could be stale with respect to page updates.
  What should we do with that? Just discard them?
  That might be a good default to start with.
  So then each rendering has some clock that should be echoed back by the handler call.
  But then if I'm going to have a clock then I should have everything be linearizable/serializable.
  That is, it should all be in one (sum) stream.
  Then the clock can be internal to serveLiveView
-}

serveLiveView :: forall r m. (Monad m) => LiveView r () -> LiveViewInputs r m -> LiveViewOutputs m
serveLiveView liveview inputs =
  let isStateS = S.map isState $ _inputStream inputs
      stateClockS = S.map (Clock . (+1)) $ numTrueBefore isStateS
      outputWithLvrS :: Stream (Of (Maybe (OutputStreamEntry), Maybe (LiveViewResult))) m ()
      outputWithLvrS = S.zipWith3 (\inp clk prevlvr -> case inp of
        InputState r -> 
            let nowlvr = getLiveViewResult r liveview
                diff = diffHtml (_html prevlvr) (_html nowlvr)
            in  (Just $ OutputPatch (diff, clk), Just nowlvr)
        InputAction (call, callClk) ->
            if callClk < clk then (Nothing, Nothing)
            else if callClk > clk then error "Oughtta be impossible: callClk > clk ???"
            else (Just $ OutputAction call, Nothing)
          ) (_inputStream inputs) stateClockS prevlvrS
      prevlvrS = mostRecentJustBefore (getLiveViewResult (_initialState inputs) liveview)
                    $ S.map snd outputWithLvrS
      initialHtml = _html $ getLiveViewResult (_initialState inputs) liveview
      liveViewResults = S.map (`getLiveViewResult` liveview) (S.cons (_initialState inputs) $ _stateStream inputs)
      htmls = S.map _html liveViewResults
      diffs = S.zipWith diffHtml htmls (S.drop 1 htmls)
   in LiveViewOutputs
        { _outputStream = S.mapMaybe fst outputWithLvrS, 
          _mountList = (toSplitText initialHtml, Clock 0),
          _firstRender = initialHtml
        }