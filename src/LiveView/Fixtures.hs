-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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

-- genStateOnlyLiveViewInputs :: (MonadGen g) => g r -> g [DepInput r] 
-- genStateOnlyLiveViewInputs genR = do
--   len <- Gen.int (Range.linear 0 100)
--   forM [0..len] $ \i -> DepState <$> genR

-- modTmpl :: (Monad m) => Int -> Int -> HtmlT m ()
-- modTmpl n modulus = "mod " <> toHtml (show modulus) <> ": " <> toHtml (show (n `mod` modulus))

-- testIntLiveView :: LiveView Int ()
-- testIntLiveView = do
--   ul_ [id_ "root"] $ do
--     i <- ask
--     li_ $ "original: " <> toHtml (show i)
--     mapM_ (li_ . modTmpl i) [2, 3, 5, 7, 10]
--     submitAction <- makeHsaction "click" "submit"
--     button_ [hsaction_ submitAction] "submit"

-- newtype ClientsidePatchError = ClientsidePatchError T.Text deriving Show

-- toClientsidePatchlist :: (Monad m) => LiveViewOutputs m -> Stream (Of ([T.Text], Clock)) (ExceptT ClientsidePatchError m) ()
-- toClientsidePatchlist outputs = S.scanM handlePatch (pure $ _mountList outputs) pure (hoist lift $ filterForPatches $ _outputStream outputs)
--   where filterForPatches = S.mapMaybe (\case OutputPatch x -> Just x; _ -> Nothing)
--         handlePatch x@(textList, Clock prev) a@(patchList, Clock next) = do
--           if next /= prev + 1
--             then throwError (ClientsidePatchError $ "Mismatched clocks: " <> tshow (x, a))
--             else pure (applyPatch textList patchList, (Clock next))
