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

module Import
  ( (&),
    tshow,
    module Control.Monad,
    module Data.Maybe,
    -- module Data.Composition,
    -- module Data.Tuple.Extra,
    txt,
  )
where

import Control.Lens ((&))
import Control.Monad
import Data.Composition
import Data.Maybe
import Data.String (IsString (..))
import Data.Text
import Data.Tuple.Extra
import NeatInterpolation (trimming)

txt = trimming

tshow :: (Show a) => a -> Text
tshow = pack . show

-- class Interp a where
--   interp :: Text -> a

-- class IsUnstring a where
--   fromUnstring :: a -> String

-- instance IsUnstring String where
--   fromUnstring = id

-- instance IsUnstring Text where
--   fronUnstring = unpack

-- instance (IsString a, IsUnstring a) => Interp a where
--   interp =

-- -- instance (Interp a, IsString b) => Interp (b -> a) where
-- --   interp = interp .: (<>)

-- -- instance (Interp a) => Interp (String -> a) where
-- --   interp = interp . pack . (uncurry (<>)) . first unpack .: (,)

-- instance (Interp a) => Interp (Bool -> a) where
--   interp = interp . (uncurry (<>)) . fmap toText .: (,)
--     where
--       toText True = "true"
--       toText False = "false"

-- instance (Interp a) => Interp (Int -> a) where
--   interp = interp . (uncurry (<>)) . fmap tshow .: (,)

-- instance (Interp a) => Interp (Char -> a) where
--   interp = interp . (uncurry (<>)) . fmap tshow .: (,)

-- i :: Interp a => a
-- i = interp ""