-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LiveView.Examples.Carcassonne.Tiles where

import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import Control.Lens
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List
import Data.Text ( Text )
import Data.String
import LiveView.Examples.Carcassonne.Types
import LiveView.Html
import Lucid
import Import
import System.Random
import System.Random.Stateful

ts :: Text -> (SideTerrain, SideTerrain, SideTerrain, SideTerrain) -> MiddleTerrain -> Int -> (Tile, Int)
ts t sides middle frequency = (Tile (fromTuple sides) middle (TileImage t 0), frequency)
  where fromTuple (u,r,d,l) = LRUD l r u d

tileSpecs :: [(Tile, Int)]
tileSpecs =
  [ ts "CCCCS" ( City, City, City, City ) (MCity True) 1
  , ts "FFFFL" ( Field, Field, Field, Field ) MMonastery 4
  , ts "FFRFL" ( Field, Field, Road, Field ) MMonastery 2
  , ts "CCFC" ( City, City, Field, City ) (MCity False) 3
  , ts "CCFCS" ( City, City, Field, City ) (MCity True) 1
  , ts "CCRC" ( City, City, Road, City ) (MCity False) 1
  , ts "CCRCS" ( City, City, Road, City ) ( MCity True ) 2
  , ts "CFFC" ( City, Field, Field, City ) (MCity False) 3
  , ts "CFFCS" ( City, Field, Field, City ) ( MCity True ) 2
  , ts "CRRC" ( City, Road, Road, City ) (MCity False) 3
  , ts "CRRCS" ( City, Road, Road, City ) (MCity True) 2
  , ts "FCFC" ( Field, City, Field, City ) (MCity False) 1
  , ts "FCFCS" ( Field, City, Field, City ) (MCity True) 2
  , ts "CFCF" ( City, Field, City, Field ) MField 3
  , ts "CCFF" ( City, City, Field, Field ) MField 2
  , ts "CFFF" ( City, Field, Field, Field ) MField 5
  , ts "CRRR" ( City, Road, Road, Road ) MField 3
  , ts "CRRF" ( City, Road, Road, Field ) MField 3
  , ts "CFRR" ( City, Field, Road, Road ) MField 3
  , ts "CRFR" ( City, Road, Field, Road ) MField 4
  , ts "RRRR" ( Road, Road, Road, Road ) MField 1
  , ts "FRRR" ( Field, Road, Road, Road ) MField 4
  , ts "RFRF" ( Road, Field, Road, Field ) MField 8
  , ts "FFRR" ( Field, Field, Road, Road ) MField 9
  ]

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle ls = do
  (fs :: [Float]) <- mapM (const $ getRandomR (0, 1)) ls
  pure $ snd <$> sortOn fst (zip fs ls)

unshuffledTiles :: [Tile]
unshuffledTiles = do
  (tile, n) <- tileSpecs
  replicate n tile

startingTile :: Tile
startingTile = Tile (from4List [Road, Road, City, Field]) MField (TileImage "CRFR" 0)

rotateCcw :: Tile -> Tile
rotateCcw (Tile (LRUD l r u d) middle image) = Tile (LRUD u d r l) middle (rotateImageCcw image)

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

computeTileBounds :: Board -> LRUD Int
computeTileBounds = HM.foldlWithKey' f (LRUD 0 0 0 0) . _xyToTile
  where f bounds (x,y) _ = bounds 
                            & lrudL %~ min x
                            & lrudR %~ max x
                            & lrudD %~ min y
                            & lrudU %~ max y

tileSize :: Text
tileSize = "80px"


(?||) :: ([Attribute] -> b) -> [[Attribute]] -> b
f ?|| as = f (as >>= id)

infixr 2 ?||

facingSides :: LRUD (Maybe Tile) -> LRUD (Maybe SideTerrain)
facingSides (LRUD nbrL nbrR nbrU nbrD) = LRUD
  (f nbrL lrudR)
  (f nbrR lrudL)
  (f nbrU lrudD)
  (f nbrD lrudU)
  where f nbr sideL = nbr ^? _Just . sides . sideL

canPlace :: Tile -> LRUD (Maybe Tile) -> Bool
canPlace t nbrh@(LRUD nbrL nbrR nbrU nbrD) =
  let nbrs = catMaybes (nbrh ^.. traverse)
      hasNbr = length nbrs > 0
      isCompat mySide theirSide
        | theirSide == Nothing = True
        | theirSide == Just mySide = True
        | otherwise = False
  in hasNbr && (all (uncurry isCompat) (zip (t ^.. sides . traverse) (facingSides nbrh ^.. traverse)))


renderBoard :: forall m r. (MonadReader r m, HasAppContext r) => HtmlT m ()
renderBoard = do
  board' <- view (appContext . gameState . board)
  mayCurrTile <- asks (\s -> s ^? appContext . gameState . gameTiles . ix 0)
  let (LRUD left right up down) = computeTileBounds board'
      xStart = left - 1
      xEnd = right + 1
      yStart = down - 1
      yEnd = up + 1
      numX = tshow $ xEnd - xStart + 1
      numY = tshow $ yEnd - yStart + 1
      spotsToRender = (,) <$> [xStart .. xEnd] <*> [yStart .. yEnd]
      neighborhood :: (Int, Int) -> LRUD (Maybe Tile)
      neighborhood (x,y) = LRUD
        (f (x - 1, y))
        (f (x + 1, y))
        (f (x, y + 1))
        (f (x, y - 1))
        where f loc = board' ^? xyToTile . ix loc
      renderSpot :: (Int, Int) -> HtmlT m ()
      renderSpot loc@(x,y) =
        let [y0, x0, y1, x1] = tshow <$> [yEnd - y + 1, x - xStart + 1, yEnd - y + 2, x - xStart + 2] in
        div_
          [ style_ [txt|
              grid-area: $y0 / $x0 / $y1 / $x1;
              width: 100%;
              height: 100%;|]
          , class_ "spot"
          ] $ div_ [class_ "tile"] $ case _xyToTile board' ^. at (x,y) of
                Nothing ->
                  let mayPlaceTileAttrs = maybeToList mayCurrTile >>=
                        (\t ->
                           if canPlace t (neighborhood loc)
                           then [ (hsaction_ (makeHsaction "click" "place_currTile"))
                                , hsvalue_ "x" (tshow x)
                                , hsvalue_ "y" (tshow y)
                                ]
                           else [])
                      canPlaceTileClass = maybe "cant-place" (const "can-place") $ mayPlaceTileAttrs ^? ix 0
                  in
                  div_ ?||
                    [ pure $ style_ [txt| grid-area: center; width: 100%; height: 100%;|]
                        , mayPlaceTileAttrs
                        , pure $ class_ canPlaceTileClass
                        ] $ ""
                      -- (fromString $ show (neighborhood loc))
                Just tile -> renderTileImage (_image tile) []
  div_ [ class_ "board"
       , style_ [txt|
          grid-template-columns: repeat($numX, $tileSize);
          grid-template-rows: repeat($numY, $tileSize);
          |]] 
    $ mapM_ renderSpot spotsToRender