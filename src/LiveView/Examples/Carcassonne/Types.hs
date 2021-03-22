{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LiveView.Examples.Carcassonne.Types where


import Control.Lens
import Data.HashMap.Strict (HashMap)
import Data.Text

data SideTerrain = City | Field | Road deriving (Show, Eq, Ord)

data MiddleTerrain = MCity {_hasCrest :: Bool} | MMonastery | MField deriving (Show, Eq, Ord)

data TileImage = TileImage 
  { _imageName :: Text
  , _imageCcwRotates :: Int
  }

makeClassy ''TileImage

data Tile = Tile
  { _sides :: [SideTerrain]
  , _middle :: MiddleTerrain
  , _image :: TileImage
  }

makeClassy ''Tile

newtype Board = Board
  { _xyToTile :: HashMap (Int, Int) Tile
  }

makeClassy ''Board

data Bounds = Bounds
  { _boundsLeft :: Int
  , _boundsRight :: Int
  , _boundsUp :: Int
  , _boundsDown :: Int
  }

makeClassy ''Bounds

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



