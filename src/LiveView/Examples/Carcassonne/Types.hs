{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Examples.Carcassonne.Types where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List.NonEmpty
import Data.Semigroup.Foldable
import Data.Text
import LiveView
import Numeric.Natural (Natural)
import StmContainers.Map qualified as StmMap
import GHC.Generics (Generic)

data SideTerrain = City | Field | Road deriving (Show, Eq, Ord)

data MiddleTerrain = MCity {_hasCrest :: Bool} | MMonastery | MField deriving (Show, Eq, Ord)

data LRUDOne = L | R | U | D deriving (Show, Eq, Ord)

data LRUD a = LRUD
  { _lrudL :: a,
    _lrudR :: a,
    _lrudU :: a,
    _lrudD :: a
  }
  deriving (Functor, Foldable, Traversable, Show)

makeClassy ''LRUD

instance Foldable1 LRUD

rotateLRUDCcw :: LRUD a -> LRUD a
rotateLRUDCcw (LRUD l r u d) = LRUD u d r l

collectLRUDOnes :: LRUD a -> NonEmpty (LRUDOne, a)
collectLRUDOnes (LRUD l r u d) =
  (L, l) :| (R, r) : (U, u) : [(D, d)]

toLRUDLens :: LRUDOne -> Lens' (LRUD a) a
toLRUDLens = \case
  L -> lrudL
  R -> lrudR
  U -> lrudU
  D -> lrudD

fromLRUDLens :: Lens' (LRUD LRUDOne) LRUDOne -> LRUDOne
fromLRUDLens l = LRUD L R U D ^. l

rotateLRUDOneCcw :: LRUDOne -> LRUDOne
rotateLRUDOneCcw = \case
  L -> D
  D -> R
  R -> U
  U -> L

newtype NumPlayers = NumPlayers Natural deriving (Num, Eq, Show)
newtype PlayerIndex = PlayerIndex Natural deriving (Num, Eq, Show, Generic)
newtype Score = Score Natural deriving (Num, Eq, Show)

instance Hashable PlayerIndex

data MeeplePlacement
  = PlaceSide LRUDOne
  | PlaceMonastery
  deriving (Show, Eq, Ord)

makeClassyPrisms ''MeeplePlacement

data TileImage = TileImage
  { _imageName :: Text,
    _imageCcwRotates :: Int
  }
  deriving (Show)

makeClassy ''TileImage

data Tile = Tile
  { _sides :: LRUD SideTerrain,
    _middle :: MiddleTerrain,
    _image :: TileImage,
    _tileMeeplePlacement :: Maybe (MeeplePlacement, PlayerIndex)
  }
  deriving (Show)

makeClassy ''Tile

newtype Board = Board
  { _xyToTile :: HashMap (Int, Int) Tile
  }

makeClassy ''Board

data TurnPhase = PhaseTile | PhasePlaceMeeple | PhaseTakeMeeple

data WhoseTurn = WhoseTurn
  { _whoseTurnPlayer :: PlayerIndex,
    _whoseTurnPhase :: TurnPhase
  }

makeClassy ''WhoseTurn


data GameState = GameState
  { _gameBoard :: Board,
    _gameTiles :: [Tile],
    _gameNumPlayers :: NumPlayers,
    _gameWhoseTurn :: WhoseTurn,
    _gameScores :: HashMap PlayerIndex Score
  }

makeClassy ''GameState

instance HasBoard GameState where
  board = gameBoard

newtype SessionId = SessionId Text deriving (Show, Eq, Ord, Hashable)

data AppContext = AppContext
  { _makeTileImageUrl :: TileImage -> Text,
    _acGameState :: GameState
  }

makeClassy ''AppContext

instance HasGameState AppContext where
  gameState = acGameState

data SessionState a = SessionState
  { _sessionChan :: STM.TChan (Maybe a),
    _sessionCurrState :: a
  }

newtype ServerContext = ServerContext
  { _scStateStore ::
      StateStore
        SessionId
        (AppContext -> AppContext)
        AppContext
  }

makeClassy ''ServerContext

instance
  HasStateStore
    ServerContext
    SessionId
    (AppContext -> AppContext)
    AppContext
  where
  stateStore = scStateStore

data Message
  = CurrentTileRotateRight
  | CurrentTileRotateLeft
  | PlaceTile (Int, Int)
  | PlaceMeeple (Int, Int) (Maybe MeeplePlacement)
  | TakeMeeple (Maybe (Int, Int))
