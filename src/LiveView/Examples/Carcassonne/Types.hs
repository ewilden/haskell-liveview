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
import GHC.Generics (Generic)
import LiveView
import Numeric.Natural (Natural)
import StmContainers.Map qualified as StmMap

data SideTerrain = City | Field | Road deriving (Show, Eq, Ord, Enum, Bounded)

data MiddleTerrain = MCity {_hasCrest :: Bool} | MMonastery | MField deriving (Show, Eq, Ord)

data LRUDOne = L | R | U | D deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance Hashable LRUDOne

flipLRUDOne :: LRUDOne -> LRUDOne
flipLRUDOne = \case
  L -> R
  R -> L
  U -> D
  D -> U


data LRUD a = LRUD
  { _lrudL :: a,
    _lrudR :: a,
    _lrudU :: a,
    _lrudD :: a
  }
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

makeLenses ''LRUD

lrudOnes :: LRUD LRUDOne
lrudOnes = LRUD L R U D

instance Applicative LRUD where
  pure a = LRUD a a a a
  (LRUD f1 f2 f3 f4) <*> (LRUD a b c d) = LRUD
    (f1 a) (f2 b) (f3 c) (f4 d)

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

newtype NumPlayers = NumPlayers Natural deriving (Num, Enum, Eq, Show)

newtype PlayerIndex = PlayerIndex {
  _unPlayerIndex :: Natural
  } deriving (Num, Eq, Show, Ord, Generic, Integral, Enum, Real)
makeLenses ''PlayerIndex

newtype Score = Score Natural deriving (Num, Eq, Show)

instance Hashable PlayerIndex

data MeeplePlacement
  = PlaceSide LRUDOne
  | PlaceMonastery
  | PlaceAbbot
  deriving (Show, Eq, Ord)

makeClassyPrisms ''MeeplePlacement

data MeepleCounts = MeepleCounts
  { _meeples :: Int,
    _abbots :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''MeepleCounts

instance Semigroup MeepleCounts where
  MeepleCounts m a <> MeepleCounts m' a' = MeepleCounts (m + m') (a + a')

instance Monoid MeepleCounts where
  mempty = MeepleCounts 0 0

data TileImage = TileImage
  { _imageName :: Text,
    _imageCcwRotates :: Int
  }
  deriving (Show, Eq, Ord)

makeClassy ''TileImage

data Tile = Tile
  { _sides :: LRUD SideTerrain,
    _middle :: MiddleTerrain,
    _image :: TileImage,
    _tileMeeplePlacement :: Maybe (MeeplePlacement, PlayerIndex)
  }
  deriving (Show, Eq, Ord)

makeClassy ''Tile

newtype Board = Board
  { _xyToTile :: HashMap (Int, Int) Tile
  } deriving Show

makeClassy ''Board

data TerrainGraphKey = TerrainGraphKey
  { _keyLoc :: (Int, Int),
    _keySide :: LRUDOne
  } | TerrainEmptyKey
  deriving (Show, Eq, Ord, Generic)

makeClassyPrisms ''TerrainGraphKey

instance Hashable TerrainGraphKey

data TurnPhase 
  = PhaseTile 
  | PhasePlaceMeeple (Int, Int) 
  | PhaseTakeAbbot
  | PhaseGameOver
  deriving (Eq, Ord, Show)


succWrapped :: (Eq a, Enum a, Bounded a) => a -> a
succWrapped a
  | a == maxBound = minBound
  | otherwise = succ a

predWrapped :: (Eq a, Enum a, Bounded a) => a -> a
predWrapped a
  | a == minBound = maxBound
  | otherwise = pred a

data WhoseTurn = WhoseTurn
  { _whoseTurnPlayer :: PlayerIndex,
    _whoseTurnPhase :: TurnPhase
  } deriving Show

makeClassy ''WhoseTurn

data GameState = GameState
  { _gameBoard :: Board,
    _gameTiles :: [Tile],
    _gameNumPlayers :: NumPlayers,
    _gameWhoseTurn :: WhoseTurn,
    _gameScores :: HashMap PlayerIndex Score
  } deriving Show

makeClassy ''GameState

instance HasBoard GameState where
  board = gameBoard

newtype SessionId = SessionId Text deriving (Show, Eq, Ord, Hashable)
newtype UserId = UserId Text deriving (Show, Eq, Ord, Hashable)

data GameRoomContext = GameRoomContext
  { _makeTileImageUrl :: TileImage -> Text,
    _grGameState :: GameState
  }
makeClassy ''GameRoomContext

instance HasGameState GameRoomContext where
  gameState = grGameState

data AppContext = AppContext
  { _acGameRoomContext :: GameRoomContext,
    _userId :: UserId
  }

makeClassy ''AppContext

instance HasGameRoomContext AppContext where
  gameRoomContext = acGameRoomContext

instance HasGameState AppContext where
  gameState = gameState

type CarcassoneStateStore = StateStore SessionId
  (GameRoomContext -> GameRoomContext)
  GameRoomContext

newtype ServerContext = ServerContext
  { _scStateStore ::
     CarcassoneStateStore
  }

makeClassy ''ServerContext

instance
  HasStateStore
    ServerContext
    SessionId
    (GameRoomContext -> GameRoomContext)
    GameRoomContext
  where
  stateStore = scStateStore

data Message
  = CurrentTileRotateRight
  | CurrentTileRotateLeft
  | PlaceTile (Int, Int)
  | PlaceMeeple (Int, Int) (Maybe MeeplePlacement)
  | TakeAbbot (Maybe (Int, Int))
