{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Examples.Carcassonne.Types where

import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM qualified as STM
import Control.Lens
import Control.Monad.Random.Strict (RandT, StdGen, MonadSplit (getSplit), evalRandT)
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List.NonEmpty
import Data.Semigroup.Foldable
import Data.Text
import GHC.Generics (Generic, Generic1)
import LiveView
import Numeric.Natural (Natural)
import StmContainers.Map qualified as StmMap
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, ToJSON1, FromJSON1, FromJSONKey)
import Web.FormUrlEncoded
import Servant.Auth.Server

data SideTerrain = City | Field | Road deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON SideTerrain
instance FromJSON SideTerrain

data MiddleTerrain = MCity {_hasCrest :: Bool} | MMonastery | MField deriving (Show, Eq, Ord, Generic)
instance ToJSON MiddleTerrain
instance FromJSON MiddleTerrain

data LRUDOne = L | R | U | D deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance Hashable LRUDOne
instance ToJSON LRUDOne
instance FromJSON LRUDOne

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
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic)

instance (ToJSON a) => ToJSON (LRUD a)
instance (FromJSON a) => FromJSON (LRUD a)

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

newtype NumPlayers = NumPlayers Natural deriving (Num, Enum, Eq, Show, Generic)

instance ToJSON NumPlayers
instance FromJSON NumPlayers

newtype PlayerIndex = PlayerIndex {
  _unPlayerIndex :: Natural
  } deriving (Num, Eq, Show, Ord, Generic, Integral, Enum, Real)
makeLenses ''PlayerIndex

newtype Score = Score Natural deriving (Num, Eq, Show, Generic)
instance ToJSON Score
instance FromJSON Score

instance Hashable PlayerIndex
instance ToJSON PlayerIndex
instance ToJSONKey PlayerIndex
instance FromJSON PlayerIndex
instance FromJSONKey PlayerIndex

data MeeplePlacement
  = PlaceSide LRUDOne
  | PlaceMonastery
  | PlaceAbbot
  deriving (Show, Eq, Ord, Generic)

makeClassyPrisms ''MeeplePlacement

instance ToJSON MeeplePlacement
instance FromJSON MeeplePlacement

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
  deriving (Show, Eq, Ord, Generic)

makeClassy ''TileImage
instance ToJSON TileImage
instance FromJSON TileImage

data Tile = Tile
  { _sides :: LRUD SideTerrain,
    _middle :: MiddleTerrain,
    _image :: TileImage,
    _tileMeeplePlacement :: Maybe (MeeplePlacement, PlayerIndex)
  }
  deriving (Show, Eq, Ord, Generic)

makeClassy ''Tile

instance ToJSON Tile
instance FromJSON Tile

newtype Board = Board
  { _xyToTile :: HashMap (Int, Int) Tile
  } deriving (Show, Generic)

instance ToJSON Board
instance FromJSON Board

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
  deriving (Eq, Ord, Show, Generic)
instance ToJSON TurnPhase
instance FromJSON TurnPhase

data TPhaseTile
data TPhasePlaceMeeple
data TPhaseTakeAbbot
data TPhaseGameOver

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
  } deriving (Show, Generic)

makeClassy ''WhoseTurn

instance ToJSON WhoseTurn
instance FromJSON WhoseTurn

data GameState = GameState
  { _gameBoard :: Board,
    _gameTiles :: [Tile],
    _gameNumPlayers :: NumPlayers,
    _gameWhoseTurn :: WhoseTurn,
    _gameScores :: HashMap PlayerIndex Score,
    _gameMostRecentError :: Text
  } deriving (Show, Generic)

makeClassy ''GameState

instance ToJSON GameState
instance FromJSON GameState

instance HasBoard GameState where
  board = gameBoard

newtype SessionId = SessionId { sessionId :: Text } deriving (Show, Eq, Ord, Hashable, Generic)
instance ToJSON SessionId
instance ToJWT SessionId
instance FromJSON SessionId
instance FromJWT SessionId
instance FromForm SessionId

newtype UserId = UserId Text deriving (Show, Eq, Ord, Hashable, Generic)
instance ToJSON UserId
instance FromJSON UserId
instance ToJSONKey UserId
instance FromJSONKey UserId

data GameRoomContext = GameRoomContext
  { _userId2Player :: HashMap UserId PlayerIndex,
    _grGameState :: GameState
  } deriving Generic
makeClassy ''GameRoomContext

instance ToJSON GameRoomContext
instance FromJSON GameRoomContext

instance HasGameState GameRoomContext where
  gameState = grGameState

data AppContext = AppContext
  { _acGameRoomContext :: GameRoomContext,
    _userId :: UserId
  } deriving Generic

makeClassy ''AppContext

instance ToJSON AppContext
instance FromJSON AppContext

myPlayerIndex :: (HasAppContext ac) => ac -> PlayerIndex
myPlayerIndex ac = ac ^?! appContext . gameRoomContext . userId2Player . ix (ac ^. userId)

instance HasGameRoomContext AppContext where
  gameRoomContext = acGameRoomContext

instance HasGameState AppContext where
  gameState = grGameState

type CarcassoneStateStore = StateStore (RandT StdGen STM) SessionId
  (GameRoomContext -> GameRoomContext)
  GameRoomContext

runRandSTMIntoIO :: RandT StdGen STM a -> IO a
runRandSTMIntoIO ma = do
  g <- getSplit
  evalRandT ma g & atomically


newtype User = User { name :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User
instance FromForm User
instance Hashable User


data ServerContext = ServerContext
  { _scStateStore ::
     CarcassoneStateStore,
    _scUserSet :: StmMap.Map User ()
  }

makeClassy ''ServerContext

instance
  HasStateStore
    ServerContext
    (RandT StdGen STM)
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

data MessageGADT phase where
  MGADTCurrentTileRotateRight :: MessageGADT TPhaseTile
  MGADTCurrentTileRotateLeft :: MessageGADT TPhaseTile
  MGADTPlaceTile :: (Int, Int) -> MessageGADT TPhaseTile
  MGADTPlaceMeeple :: (Int, Int) -> Maybe MeeplePlacement -> MessageGADT TPhasePlaceMeeple
  MGADTTakeAbbot :: Maybe (Int, Int) -> MessageGADT TPhaseTakeAbbot
