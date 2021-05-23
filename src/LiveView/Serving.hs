{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module LiveView.Serving where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Lens qualified as L
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Monoidal (MonoidalHashMap)
import Data.HashMap.Strict hiding ((!?))
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.IORef
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Import
import LiveView.Html
import Lucid qualified as L
import Streaming
import Streaming.Prelude qualified as S
import Control.Monad.Writer.Strict (WriterT)
import Data.IntMap.Monoidal.Strict (MonoidalIntMap)
import Data.IntMap.Monoidal.Strict qualified as MonoidalIntMap

newtype Clock = Clock
  { _unClock :: Int
  }
  deriving (Eq, Hashable, Enum, FromJSON, Ord, Show, ToJSON)

data ActionCall = ActionCall
  { _action :: T.Text,
    _payload :: HashMap T.Text T.Text
  }
  deriving (Show)

deriveFromJSON (defaultOptions {fieldLabelModifier = drop 1}) ''ActionCall

mergePar :: Stream (Of a) IO () -> Stream (Of a) IO () -> Stream (Of a) IO ()
mergePar a b = do
  chan <- liftIO STM.newTChanIO
  let writeThread stream =
        S.mapM_
          (atomically . STM.writeTChan chan . Just)
          stream
          >> atomically (STM.writeTChan chan Nothing)
  a' <- liftIO $ Async.async (writeThread a)
  b' <- liftIO $ Async.async (writeThread b)
  liftIO $ Async.link a'
  liftIO $ Async.link b'
  let go = do
        a'' <- liftIO $ Async.poll a'
        b'' <- liftIO $ Async.poll b'
        case (a'', b'') of
          (Just _, _) -> liftIO $ void $ Async.waitEither a' b'
          (_, Just _) -> liftIO $ void $ Async.waitEither a' b'
          (Nothing, Nothing) -> do
            el <- liftIO $ atomically $ STM.readTChan chan
            case el of
              Nothing -> pure ()
              Just x -> S.yield x
            go
  go

commuteState :: (Monad m) => s -> Stream (Of a) (StateT s m) r -> Stream (Of a) m (r, s)
commuteState s stream =
  runStateT (distribute stream) s

