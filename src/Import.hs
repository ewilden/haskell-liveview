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