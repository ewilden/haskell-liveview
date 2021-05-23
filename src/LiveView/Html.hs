{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Html (
  module LiveView.Html,
  module Lucid
  ) where

import Control.Lens hiding ((.=))
import Control.Lens qualified as L
import Data.Aeson
import Data.Algorithm.Diff
import Data.HashMap.Strict hiding ((!?))
import Data.Hashable
import Data.List (foldl')
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text hiding (foldl', length, replicate, singleton, zip)
import Data.Vector hiding (length, replicate, singleton, toList, zip)
import Data.Vector qualified as V
import Import
import Lucid qualified
import Lucid qualified as L
import Lucid.Base qualified as L
import NeatInterpolation

foo :: L.Html ()
foo = L.div_ $ L.ul_ [L.class_ "foo"] $ mconcat $ replicate 3 (L.li_ "item")

bar :: L.Html ()
bar = L.ul_ $ L.li_ $ mconcat $ replicate 4 (L.span_ "item")

splitBetweenTags :: Text -> [Text]
splitBetweenTags txt =
  let chunks = splitOn "><" txt
      n = length chunks
      idxChunks = zip [1 ..] chunks
      finalizeChunk (i, chunk)
        | i == 1 = chunk <> ">"
        | i == n = "<" <> chunk
        | otherwise = "<" <> chunk <> ">"
   in if n == 1 then chunks else finalizeChunk <$> idxChunks

toSplitText :: L.Html () -> [Text]
toSplitText = splitBetweenTags . TL.toStrict . L.renderText

diffHtml :: L.Html () -> L.Html () -> [PatchEntry Text]
diffHtml l r = toPatch $ getDiff (toSplitText l) (toSplitText r)

toPatchEntry :: Diff a -> PatchEntry a
toPatchEntry (First _) = Delete
toPatchEntry (Second a) = Insert a
toPatchEntry (Both _ _) = Keep

toPatch :: [Diff a] -> [PatchEntry a]
toPatch = fmap toPatchEntry

applyPatch :: [a] -> [PatchEntry a] -> [a]
applyPatch (a : as) (Delete : ps) = applyPatch as ps
applyPatch (a : as) (Keep : ps) = a : applyPatch as ps
applyPatch as ((Insert a') : ps) = a' : applyPatch as ps
applyPatch [] [] = []
applyPatch _ [] = error "Ran out of patch entries"
applyPatch [] _ = error "Ran out of input list"

data PatchEntry a = Delete | Keep | Insert a deriving (Show, Eq)

instance (ToJSON a) => ToJSON (PatchEntry a) where
  toJSON Delete = toJSON @Int 0
  toJSON Keep = toJSON @Int 1
  toJSON (Insert a) = toJSON [toJSON a]

instance (FromJSON a) => FromJSON (PatchEntry a) where
  parseJSON (Array valvec) = case valvec !? 0 of
    Nothing -> fail "unexpected empty array while trying to read Insert"
    Just value -> Insert <$> parseJSON value
  parseJSON (Number 0) = pure Delete
  parseJSON (Number 1) = pure Keep
  parseJSON _ = mzero

newtype Hsaction = Hsaction
  { _event2action :: HashMap T.Text T.Text
  }
  deriving (Semigroup, Monoid)

makeLenses ''Hsaction

buildHsactionText :: Hsaction -> T.Text
buildHsactionText hsaction =
  _event2action hsaction & toList
    <&> (\(eventName, actionName) -> eventName <> ":" <> actionName)
    & T.intercalate ";"

hsaction_ :: Hsaction -> L.Attribute
hsaction_ = L.makeAttribute "hsaction" . buildHsactionText

hsactions_ :: [Hsaction] -> L.Attribute
hsactions_ = hsaction_ . mconcat

makeHsaction :: T.Text -> T.Text -> Hsaction
makeHsaction event action = Hsaction $ singleton event action

{-

BINDING	ATTRIBUTES
Params	phx-value-*
Click Events	phx-capture-click, phx-click
Focus/Blur Events	phx-window-focus, phx-window-blur, phx-focus, phx-blur
Key Events	phx-key, phx-window-keyup, phx-window-keydown, phx-keyup, phx-keydown
Form Events	phx-auto-recover, phx-trigger-action, phx-disable-with, phx-feedback-for, phx-submit, phx-change
Rate Limiting	phx-throttle, phx-debounce
DOM Patching	phx-update
JS Interop	phx-hook

-}

hsvalue_ :: Text -> Text -> L.Attribute
hsvalue_ attrName value = L.makeAttribute [trimming|hsvalue-$attrName|] value

hskey_ :: Text -> L.Attribute
hskey_ = L.makeAttribute "hskey"

hsthrottle_ :: Rational -> L.Attribute
hsthrottle_ = L.makeAttribute "hsthrottle" . tshow

hsdebounce_ :: Rational -> L.Attribute
hsdebounce_ = L.makeAttribute "hsdebounce" . tshow

hsdebounceBlur_ :: L.Attribute
hsdebounceBlur_ = L.makeAttribute "hsdebounce" "blur"

hsprevent_ :: Text -> L.Attribute
hsprevent_ = L.makeAttribute "hsprevent"
