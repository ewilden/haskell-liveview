{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Html where

import Control.Lens hiding ((.=))
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import Data.Algorithm.Diff
import Data.HashMap.Strict hiding ((!?))
import Data.Hashable
import Data.List (foldl')
import Data.Text.Lazy hiding (foldl', length, replicate, zip)
import Data.Vector hiding (length, replicate, zip)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Import
import Lucid qualified as L

foo :: L.Html ()
foo = L.div_ $ L.ul_ $ mconcat $ replicate 3 (L.li_ "item")

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
toSplitText = splitBetweenTags . L.renderText

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

newtype HandlerId = HandlerId Int deriving (Eq, Hashable)

-- instance Hashable HandlerId

newtype JsonMask = JsonMask {_unJsonMask :: Value}

makeLenses ''JsonMask

data Handler msg = Handler
  { _jsonMask :: JsonMask,
    _msgBuilder :: Value -> msg
  }

makeLenses ''Handler

data LiveInternalState msg = LiveInternalState
  { _uid :: Int,
    _id2handler :: HashMap HandlerId (Value -> msg)
  }

makeLenses ''LiveInternalState

makeHandlerId :: (MonadState (LiveInternalState msg) m) => m HandlerId
makeHandlerId = HandlerId <$> (uid <<%= (+ 1))

-- (LiveInternalState i) <- get
-- put $ LiveInternalState $ i + 1
-- return i

makeHandler :: (MonadState (LiveInternalState msg) m) => (Value -> msg) -> m HandlerId
makeHandler f = do
  handlerId <- makeHandlerId
  id2handler . at handlerId L..= Just f
  return handlerId

newtype LiveM msg r a = LiveM
  { runLiveM :: ReaderT r (State (LiveInternalState msg)) a
  }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadState (LiveInternalState msg))

type LiveView msg r a = L.HtmlT (LiveM msg r) a
