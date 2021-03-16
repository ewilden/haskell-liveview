{-# LANGUAGE OverloadedStrings #-}

module LiveView.Fixtures where

import Import
import Hedgehog
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Algorithm.Diff
import Data.String (IsString, fromString)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LiveView.Html
import Data.Text qualified as T
import LiveView.Serving
import Lucid
import Streaming
import qualified Streaming.Prelude as S

genStateOnlyLiveViewInputs :: (MonadGen g) => g r -> g [DepInput r] 
genStateOnlyLiveViewInputs genR = do
  len <- Gen.int (Range.linear 0 100)
  forM [0..len] $ \i -> DepState <$> genR

modTmpl :: (Monad m) => Int -> Int -> HtmlT m ()
modTmpl n modulus = "mod " <> toHtml (show modulus) <> ": " <> toHtml (show (n `mod` modulus))

testIntLiveView :: LiveView Int ()
testIntLiveView = do
  ul_ [id_ "root"] $ do
    i <- ask
    li_ $ "original: " <> toHtml (show i)
    mapM_ (li_ . modTmpl i) [2, 3, 5, 7, 10]
    submitAction <- makeHsaction "click" "submit"
    button_ [hsaction_ submitAction] "submit"

newtype ClientsidePatchError = ClientsidePatchError T.Text deriving Show

-- toClientsidePatchlist :: (Monad m) => LiveViewOutputs m -> Stream (Of ([T.Text], Clock)) (ExceptT ClientsidePatchError m) ()
-- toClientsidePatchlist outputs = S.scanM handlePatch (pure $ _mountList outputs) pure (hoist lift $ filterForPatches $ _outputStream outputs)
--   where filterForPatches = S.mapMaybe (\case OutputPatch x -> Just x; _ -> Nothing)
--         handlePatch x@(textList, Clock prev) a@(patchList, Clock next) = do
--           if next /= prev + 1
--             then throwError (ClientsidePatchError $ "Mismatched clocks: " <> tshow (x, a))
--             else pure (applyPatch textList patchList, (Clock next))
