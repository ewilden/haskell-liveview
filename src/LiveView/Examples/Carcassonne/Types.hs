{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Examples.Carcassonne.Types where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Text
import LiveView
import StmContainers.Map qualified as StmMap

data SideTerrain = City | Field | Road deriving (Show, Eq, Ord)

data MiddleTerrain = MCity {_hasCrest :: Bool} | MMonastery | MField deriving (Show, Eq, Ord)

data LRUD a = LRUD
  { _lrudL :: a,
    _lrudR :: a,
    _lrudU :: a,
    _lrudD :: a
  }
  deriving (Functor, Foldable, Traversable, Show)

makeClassy ''LRUD

data TileImage = TileImage
  { _imageName :: Text,
    _imageCcwRotates :: Int
  }
  deriving (Show)

makeClassy ''TileImage

data Tile = Tile
  { _sides :: LRUD SideTerrain,
    _middle :: MiddleTerrain,
    _image :: TileImage
  }
  deriving (Show)

makeClassy ''Tile

newtype Board = Board
  { _xyToTile :: HashMap (Int, Int) Tile
  }

makeClassy ''Board

data GameState = GameState
  { _gameBoard :: Board,
    _gameTiles :: [Tile]
  }

makeClassy ''GameState

instance HasBoard GameState where
  board = gameBoard

newtype SessionId = SessionId Text deriving (Show, Eq, Ord, Hashable)

data AppContext = AppContext
  { _makeTileImageUrl :: TileImage -> Text,
    _acGameState :: GameState,
    _sessionId :: Text
  }

makeClassy ''AppContext

instance HasGameState AppContext where
  gameState = acGameState

data SessionState a = SessionState
  { _sessionChan :: STM.TChan (Maybe a),
    _sessionCurrState :: a
  }

newtype ServerContext = ServerContext
  { _scStateStore :: StateStore SessionId AppContext
  }

makeClassy ''ServerContext

instance HasStateStore ServerContext SessionId AppContext where
  stateStore = scStateStore
