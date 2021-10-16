{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LiveView.Examples.Carcassonne.Tiles where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Data.HashMap.Strict qualified as HM
import Data.List
import Data.String
import Data.Text (Text)
import Import
import LiveView
import LiveView.Examples.Carcassonne.Types
import LiveView.Html
import Lucid

ts :: Text -> (SideTerrain, SideTerrain, SideTerrain, SideTerrain) -> MiddleTerrain -> Int -> (Tile, Int)
ts t sides middle frequency =
  (Tile (fromTuple sides) middle (TileImage t 0) Nothing, frequency)
  where
    fromTuple (u, r, d, l) = LRUD l r u d

tileSpecs :: [(Tile, Int)]
tileSpecs =
  [ ts "CCCCS" (City, City, City, City) (MCity True) 1,
    ts "FFFFL" (Field, Field, Field, Field) MMonastery 4,
    ts "FFRFL" (Field, Field, Road, Field) MMonastery 2,
    ts "CCFC" (City, City, Field, City) (MCity False) 3,
    ts "CCFCS" (City, City, Field, City) (MCity True) 1,
    ts "CCRC" (City, City, Road, City) (MCity False) 1,
    ts "CCRCS" (City, City, Road, City) (MCity True) 2,
    ts "CFFC" (City, Field, Field, City) (MCity False) 3,
    ts "CFFCS" (City, Field, Field, City) (MCity True) 2,
    ts "CRRC" (City, Road, Road, City) (MCity False) 3,
    ts "CRRCS" (City, Road, Road, City) (MCity True) 2,
    ts "FCFC" (Field, City, Field, City) (MCity False) 1,
    ts "FCFCS" (Field, City, Field, City) (MCity True) 2,
    ts "CFCF" (City, Field, City, Field) MField 3,
    ts "CCFF" (City, City, Field, Field) MField 2,
    ts "CFFF" (City, Field, Field, Field) MField 5,
    ts "CRRR" (City, Road, Road, Road) MField 3,
    ts "CRRF" (City, Road, Road, Field) MField 3,
    ts "CFRR" (City, Field, Road, Road) MField 3,
    ts "CRFR" (City, Road, Field, Road) MField 4,
    ts "RRRR" (Road, Road, Road, Road) MField 1,
    ts "FRRR" (Field, Road, Road, Road) MField 4,
    ts "RFRF" (Road, Field, Road, Field) MField 8,
    ts "FFRR" (Field, Field, Road, Road) MField 9
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
startingTile =
  Tile (LRUD Road Road City Field) MField (TileImage "CRFR" 0) Nothing

straightRoadTile :: Tile
straightRoadTile = fst $ ts "RFRF" (Road, Field, Road, Field) MField 8

rotateCcw :: Tile -> Tile
rotateCcw (Tile (LRUD l r u d) middle image mplPlace) =
  Tile
    (LRUD u d r l)
    middle
    (rotateImageCcw image)
    (mplPlace & _Just . _1 . _PlaceSide %~ rotateLRUDOneCcw)

rotateImageCcw :: TileImage -> TileImage
rotateImageCcw image = image & imageCcwRotates %~ (\x -> (x + 1) `mod` 4)

rotateCw :: Tile -> Tile
rotateCw = rotateCcw . rotateCcw . rotateCcw

renderTileImage :: (MonadReader r m, HasAppContext r) => TileImage -> [Attribute] -> HtmlT m ()
renderTileImage tileImage attrs = do
  genImageUrl <- asks (^. makeTileImageUrl)
  img_ $ [class_ "tile-image", src_ (genImageUrl tileImage)] ++ attrs

renderTile :: (HasAppContext r) => Tile -> [Text] -> LiveView r a
renderTile tile classes =
  div_ [classes_ $ "tile" : classes] $
    renderTileContents tile

computeTileBounds :: Board -> LRUD Int
computeTileBounds = HM.foldlWithKey' f (LRUD 0 0 0 0) . _xyToTile
  where
    f bounds (x, y) _ =
      bounds
        & lrudL %~ min x
        & lrudR %~ max x
        & lrudD %~ min y
        & lrudU %~ max y

tileSize :: Text
tileSize = "80px"

(?||) :: ([Attribute] -> b) -> [[Attribute]] -> b
f ?|| as = f (join as)

infixr 2 ?||

isMCity :: MiddleTerrain -> Bool
isMCity (MCity _) = True
isMCity _ = False

sidesWithIsTerminus :: Tile -> LRUD (SideTerrain, Bool)
sidesWithIsTerminus tile =
  let terrainCount :: SideTerrain -> Int
      terrainCount terrain = length $ filter (== terrain) $ tile ^.. sides . traverse
      terrainToIsTerminus :: SideTerrain -> Bool
      terrainToIsTerminus = \case
        City -> not $ isMCity $ tile ^. middle
        Road -> terrainCount Road /= 2
        _ -> False
   in (\t -> (t, terrainToIsTerminus t)) <$> tile ^. sides

facingSidesWithIsTerminus :: LRUD (Maybe Tile) -> LRUD (Maybe (SideTerrain, Bool))
facingSidesWithIsTerminus tileNeighbors =
  let facingLenses = lrudOnes <&> flipLRUDOne <&> toLRUDLens
      mkPair mayNbr sideL = do
        nbr <- mayNbr
        pure $ sidesWithIsTerminus nbr ^. sideL
   in mkPair <$> tileNeighbors <*> facingLenses

facingSides :: LRUD (Maybe Tile) -> LRUD (Maybe SideTerrain)
facingSides tileNeighbors =
  let facingLenses = lrudOnes <&> flipLRUDOne <&> toLRUDLens
   in (\nbr sideL -> nbr ^? _Just . sides . sideL)
        <$> tileNeighbors <*> facingLenses

canPlace :: Tile -> LRUD (Maybe Tile) -> Bool
canPlace t nbrh@(LRUD nbrL nbrR nbrU nbrD) =
  let nbrs = catMaybes (nbrh ^.. traverse)
      hasNbr = not (null nbrs)
      isCompat mySide theirSide
        | isNothing theirSide = True
        | theirSide == Just mySide = True
        | otherwise = False
   in hasNbr && all (uncurry isCompat) (zip (t ^.. sides . traverse) (facingSides nbrh ^.. traverse))

tileNeighborhood :: (HasBoard b) => (Int, Int) -> b -> LRUD (Maybe Tile)
tileNeighborhood loc b = f <$> (lrudNeighbors <*> pure loc)
  where
    f loc = b ^? xyToTile . ix loc

canPlaceOnBoard :: (HasBoard b) => ((Int, Int), Tile) -> b -> Bool
canPlaceOnBoard (loc, tile) b =
  let nbrh = tileNeighborhood loc b
  in canPlace tile nbrh

lrudNeighbors :: LRUD ((Int, Int) -> (Int, Int))
lrudNeighbors =
  LRUD
    (_1 -~ 1)
    (_1 +~ 1)
    (_2 +~ 1)
    (_2 -~ 1)

renderBoard' :: forall r. (HasAppContext r) => LiveView r Message
renderBoard' = do
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
      neighborhood (x, y) =
        f
          <$> LRUD
            (x - 1, y)
            (x + 1, y)
            (x, y + 1)
            (x, y - 1)
        where
          f loc = board' ^? xyToTile . ix loc
      renderSpot :: (Int, Int) -> LiveView r Message
      renderSpot loc@(x, y) =
        let [y0, x0, y1, x1] = tshow <$> [yEnd - y + 1, x - xStart + 1, yEnd - y + 2, x - xStart + 2]
         in div_
              [ style_
                  [txt|
              grid-area: $y0 / $x0 / $y1 / $x1;
              width: 100%;
              height: 100%;|],
                class_ "spot"
              ]
              $ div_ [class_ "tile"] $ case _xyToTile board' ^. at (x, y) of
                Nothing -> do
                  mayPlaceCurrTile <- do
                    case mayCurrTile <&> (\t -> (t, canPlace t (neighborhood loc))) of
                      Just (currTile, True) ->
                        Just
                          <$> addActionBinding
                            "click"
                            (\_ -> PlaceTile loc)
                      _ -> pure Nothing
                  let canPlaceTileClass = maybe "cant-place" (const "can-place") mayPlaceCurrTile
                  div_
                    ?|| [ pure $ style_ [txt| grid-area: center; width: 100%; height: 100%;|],
                          maybeToList $ hsaction_ <$> mayPlaceCurrTile,
                          pure $ class_ canPlaceTileClass
                        ]
                    $ ""
                -- (fromString $ show (neighborhood loc))
                Just tile -> do
                  renderTileContents tile
  div_
    [ class_ "board",
      style_
        [txt|
          grid-template-columns: repeat($numX, $tileSize);
          grid-template-rows: repeat($numY, $tileSize);
          |]
    ]
    $ mapM_ renderSpot spotsToRender

renderTileContents :: (HasAppContext r) => Tile -> LiveView r a
renderTileContents tile = do
  renderTileImage (_image tile) []
  case tile ^. tileMeeplePlacement of
    Nothing -> ""
    Just (mplace, playerIndex) -> do
      let gridArea = case mplace of
            PlaceMonastery -> "2 / 2"
            PlaceAbbot -> "2 / 2"
            PlaceSide L -> "2 / 1"
            PlaceSide R -> "2 / 3"
            PlaceSide U -> "1 / 2"
            PlaceSide D -> "3 / 2"
      div_ [class_ "meeple-container"] $ div_ [style_ [txt|
                          grid-area: $gridArea;
                          text-align: center;
                          |]] $ fromString $ show playerIndex
