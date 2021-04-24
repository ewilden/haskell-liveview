{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Examples.Carcassonne.Types where


import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Text

data SideTerrain = City | Field | Road deriving (Show, Eq, Ord)

data MiddleTerrain = MCity {_hasCrest :: Bool} | MMonastery | MField deriving (Show, Eq, Ord)

data LRUD a = LRUD
  { _lrudL :: a
  , _lrudR :: a
  , _lrudU :: a
  , _lrudD :: a
  } deriving (Functor, Foldable, Traversable, Show)

makeClassy ''LRUD

from4Tuple :: (a,a,a,a) -> LRUD a
from4Tuple (a,b,c,d) = LRUD a b c d

from4List :: [a] -> LRUD a
from4List [l, r, u, d] = LRUD l r u d

data TileImage = TileImage 
  { _imageName :: Text
  , _imageCcwRotates :: Int
  } deriving Show

makeClassy ''TileImage

data Tile = Tile
  { _sides :: LRUD SideTerrain
  , _middle :: MiddleTerrain
  , _image :: TileImage
  } deriving Show

makeClassy ''Tile

newtype Board = Board
  { _xyToTile :: HashMap (Int, Int) Tile
  }

makeClassy ''Board

data GameState = GameState
  { _gameBoard :: Board
  , _gameTiles :: [Tile]
  }

makeClassy ''GameState

instance HasBoard GameState where
  board = gameBoard

data AppContext = AppContext
  { _makeTileImageUrl :: TileImage -> Text
  , _acGameState :: GameState
  }

makeClassy ''AppContext

instance HasGameState AppContext where
  gameState = acGameState



