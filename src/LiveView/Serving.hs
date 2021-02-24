module LiveView.Serving where

import Control.Lens hiding ((.=))
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
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

-- runlv :: LiveView msg r () -> ReaderT r (State (LiveInternalState msg)) (L.Html ())
-- runlv = undefined

newtype Clock = Clock Int deriving (Eq, Hashable, Enum, Ord)

data InputStreamEntry r 
  = InputState r
  | InputJson (HandlerCall, Clock)

isState :: InputStreamEntry r -> Bool
isState (InputState _) = True
isState _ = False

numTrueBefore :: Monad m => Stream (Of Bool) m r -> Stream (Of Int) m r
numTrueBefore = S.drop 1 . S.scan (\x a -> x + fromEnum a) 0 id

mostRecentJustBefore :: Monad m => a -> Stream (Of (Maybe a)) m r -> Stream (Of a) m r
mostRecentJustBefore a = S.scan (\x mayA -> case mayA of
    Nothing -> x
    Just x' -> x') a id

-- data SumStreamEntry r = RenderState r | Json (HandlerCall, Clock)

data LiveViewInputs msg r m = LiveViewInputs
  { _initialState :: r,
    -- _stateStream :: Stream (Of r) m (),
    -- _jsonStream :: Stream (Of (HandlerCall, Clock)) m (),
    _inputStream :: Stream (Of (InputStreamEntry r)) m ()
  }

_stateStream :: (Monad m) => LiveViewInputs msg r m -> Stream (Of r) m ()
_stateStream inputs = _inputStream inputs 
  & S.mapMaybe (\case InputState r -> Just r; _ -> Nothing)

_jsonStream :: (Monad m) => LiveViewInputs msg r m -> Stream (Of (HandlerCall, Clock)) m ()
_jsonStream inputs = _inputStream inputs 
  & S.mapMaybe (\case InputJson x -> Just x; _ -> Nothing)

data OutputStreamEntry msg
  = OutputMsg msg
  | OutputPatch ([PatchEntry T.Text], Clock)

data LiveViewOutputs msg m = LiveViewOutputs
  { _outputStream :: Stream (Of (OutputStreamEntry msg)) m (),
    -- _msgs :: Stream (Of msg) m (),
    -- _patches :: Stream (Of ([PatchEntry T.Text], Clock)) m (),
    _mountList :: ([T.Text], Clock),
    _firstRender :: T.Text
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

serveLiveView :: forall msg r m. (Monad m) => LiveView msg r () -> LiveViewInputs msg r m -> LiveViewOutputs msg m
serveLiveView liveview inputs =
  let isStateS = S.map isState $ _inputStream inputs
      stateClockS = S.map (Clock . (+1)) $ numTrueBefore isStateS
      outputWithLvrS :: Stream (Of (Maybe (OutputStreamEntry msg), Maybe (LiveViewResult msg))) m ()
      outputWithLvrS = S.zipWith3 (\inp clk prevlvr -> case inp of
        InputState r -> 
            let nowlvr = getLiveViewResult r liveview
                diff = diffHtml (_html prevlvr) (_html nowlvr)
            in  (Just $ OutputPatch (diff, clk), Just nowlvr)
        InputJson (call, callClk) ->
            if callClk < clk then (Nothing, Nothing)
            else if callClk > clk then error "Oughtta be impossible: callClk > clk ???"
            else ((do
              handler <- HM.lookup (_handlerId call) (_id2handler $ _liveInternalState prevlvr)
              pure $ OutputMsg $ _msgBuilder handler $ _maskedJson call), Nothing)
          ) (_inputStream inputs) stateClockS prevlvrS
      prevlvrS = mostRecentJustBefore (getLiveViewResult (_initialState inputs) liveview)
                    $ S.map snd outputWithLvrS
      initialHtml = _html $ getLiveViewResult (_initialState inputs) liveview
      liveViewResults = S.map (flip getLiveViewResult liveview) (S.cons (_initialState inputs) $ _stateStream inputs)
      htmls = S.map _html liveViewResults
      diffs = S.zipWith (diffHtml) htmls (S.drop 1 htmls)
   in LiveViewOutputs
        { _outputStream = S.mapMaybe fst outputWithLvrS, 
          _mountList = (toSplitText initialHtml, Clock 0),
          _firstRender = TL.toStrict $ L.renderText initialHtml
        }