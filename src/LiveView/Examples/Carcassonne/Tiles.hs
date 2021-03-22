{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LiveView.Examples.Carcassonne.Tiles where

import Control.Lens
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text ( Text )
import LiveView.Examples.Carcassonne.Types
import Lucid
import Import

ts :: Text -> [SideTerrain] -> MiddleTerrain -> Int -> (Tile, Int)
ts t sides middle frequency = (Tile sides middle (TileImage t 0), frequency)

tileSpecs :: [(Tile, Int)]
tileSpecs =
  [ ts "CCCCS" [ City, City, City, City ] (MCity True) 1
  , ts "FFFFL" [ Field, Field, Field, Field ] MMonastery 4
  , ts "FFRFL" [ Field, Field, Road, Field ] MMonastery 2
  , ts "CCFC" [ City, City, Field, City ] (MCity False) 3
  , ts "CCFCS" [ City, City, Field, City ] (MCity True) 1
  , ts "CCRC" [ City, City, Road, City ] (MCity False) 1
  , ts "CCRCS" [ City, City, Road, City ] ( MCity True ) 2
  , ts "CFFC" [ City, Field, Field, City ] (MCity False) 3
  , ts "CFFCS" [ City, Field, Field, City ] ( MCity True ) 2
  , ts "CRRC" [ City, Road, Road, City ] (MCity False) 3
  , ts "CRRCS" [ City, Road, Road, City ] (MCity True) 2
  , ts "FCFC" [ Field, City, Field, City ] (MCity False) 1
  , ts "FCFCS" [ Field, City, Field, City ] (MCity True) 2
  , ts "CFCF" [ City, Field, City, Field ] MField 3
  , ts "CCFF" [ City, City, Field, Field ] MField 2
  , ts "CFFF" [ City, Field, Field, Field ] MField 5
  , ts "CRRR" [ City, Road, Road, Road ] MField 3
  , ts "CRRF" [ City, Road, Road, Field ] MField 3
  , ts "CFRR" [ City, Field, Road, Road ] MField 3
  , ts "CRFR" [ City, Road, Field, Road ] MField 4
  , ts "RRRR" [ Road, Road, Road, Road ] MField 1
  , ts "FRRR" [ Field, Road, Road, Road ] MField 4
  , ts "RFRF" [ Road, Field, Road, Field ] MField 8
  , ts "FFRR" [ Field, Field, Road, Road ] MField 9
  ]

startingTile :: Tile
startingTile = Tile [City, Road, Field, Road] MField (TileImage "CRFR" 0)

rotateCcw :: Tile -> Tile
rotateCcw (Tile sides middle image) = Tile (drop 1 sides ++ take 1 sides) middle (rotateImageCcw image)

rotateImageCcw :: TileImage -> TileImage 
rotateImageCcw image = image & imageCcwRotates %~ (\x -> (x + 1) `mod` 4)

rotateCw :: Tile -> Tile
rotateCw = rotateCcw . rotateCcw . rotateCcw

renderTileImage :: (MonadReader r m, HasAppContext r) => TileImage -> [Attribute] -> HtmlT m ()
renderTileImage tileImage attrs = do
  genImageUrl <- asks (^. makeTileImageUrl)
  img_ $ [class_ "tile-image", src_ (genImageUrl tileImage)] ++ attrs

renderTile :: (MonadReader r m, HasAppContext r) => Tile -> [Text] -> HtmlT m ()
renderTile tile classes = div_ [classes_ $ "tile": classes] $ 
  renderTileImage (_image tile) []

computeTileBounds :: Board -> Bounds
computeTileBounds = HM.foldlWithKey' f (Bounds 0 0 0 0) . _xyToTile
  where f bounds (x,y) _ = bounds 
                            & boundsLeft %~ min x
                            & boundsRight %~ max x
                            & boundsDown %~ min y
                            & boundsUp %~ max y

tileSize :: Text
tileSize = "80px"

renderBoard :: forall m r. (MonadReader r m, HasAppContext r) => HtmlT m ()
renderBoard = do
  board' <- view (appContext . gameState . board)
  let (Bounds left right up down) = computeTileBounds board'
      xStart = left - 1
      xEnd = right + 1
      yStart = down - 1
      yEnd = up + 1
      numX = tshow $ xEnd - xStart + 1
      numY = tshow $ yEnd - yStart + 1
      spotsToRender = (,) <$> [xStart .. xEnd] <*> [yStart .. yEnd]
      renderSpot :: (Int, Int) -> HtmlT m ()
      renderSpot (x,y) = 
        let [y0, x0, y1, x1] = tshow <$> [yEnd - y + 1, x - xStart + 1, yEnd - y + 2, x - xStart + 2] in
        div_
          [ style_ [txt|
              grid-area: $y0 / $x0 / $y1 / $x1|]
          , class_ "spot"
          ] $ div_ [class_ "tile"] $ case _xyToTile board' ^. at (x,y) of
                Nothing -> div_ 
                  [ style_ [txt| grid-area: center;|]
                  ] ""
                Just tile -> renderTileImage (_image tile) []
  div_ [ class_ "board"
       , style_ [txt|
          grid-template-columns: repeat($numX, $tileSize);
          grid-template-rows: repeat($numY, $tileSize);
          |]] 
    $ mapM_ renderSpot spotsToRender
