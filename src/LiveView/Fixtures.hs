{-# LANGUAGE OverloadedStrings #-}

module LiveView.Fixtures where

import Import
import Hedgehog
import Control.Monad.Reader
import Data.Aeson
import Data.Algorithm.Diff
import Data.String (IsString, fromString)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LiveView.Html
import LiveView.Serving
import Lucid
import Streaming
import qualified Streaming.Prelude as S

-- genInputStreamEntry :: (MonadGen g) => g r -> g (InputStreamEntry r)
-- genInputStreamEntry genR = 

genStateOnlyLiveViewInputs :: (MonadGen g) => g r -> g [InputStreamEntry r] 
genStateOnlyLiveViewInputs genR = do
  len <- Gen.int (Range.linear 0 100)
  forM [0..len] $ \i -> InputState <$> genR

modTmpl :: (Monad m) => Int -> Int -> HtmlT m ()
modTmpl n modulus = "mod " <> toHtml (show modulus) <> ": " <> toHtml (show (n `mod` modulus))

testIntLiveView :: LiveView () Int ()
testIntLiveView = do
  ul_ [id_ "root"] $ do
    i <- ask
    li_ $ "original: " <> toHtml (show i)
    mapM_ (li_ . modTmpl i) [2, 3, 5, 7, 10]
    submitAction <- makeHsaction (Handler (JsonMask (Object mempty)) (const ()) "click")
    button_ [hsaction_ submitAction] "submit"

