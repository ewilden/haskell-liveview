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
import Lucid.Base

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

oneCityThreeRoadsTile :: Tile
oneCityThreeRoadsTile = fst $ ts "CRRR" (City, Road, Road, Road) MField 3

oneCityThreeFieldsTile :: Tile
oneCityThreeFieldsTile = fst $ ts "CFFF" (City, Field, Field, Field) MField 5

twoRoadsTile :: Tile
twoRoadsTile = fst $ ts "RFRF" (Road, Field, Road, Field) MField 8

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

makeTileImageUrl :: TileImage -> Text
makeTileImageUrl (TileImage name ccwRotates) =
        let suffix = case ccwRotates `mod` 4 of
              0 -> ""
              n -> "-" <> tshow n
          in [txt|/tiles/${name}50${suffix}.jpg|]

renderTileImage :: (MonadReader r m, HasAppContext r) => TileImage -> [Attribute] -> HtmlT m ()
renderTileImage tileImage attrs = do
  img_ $ [class_ "tile-image", src_ (makeTileImageUrl tileImage)] ++ attrs

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
canPlace t nbrh =
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

renderBoard' :: forall r. (HasAppContext r, HasGameState r) => LiveView r Message
renderBoard' = do
  board' <- view (gameState . board)
  mayCurrTile <- asks (\s -> s ^? gameState . gameTiles . ix 0)
  isPhasePlaceTile <- view (gameWhoseTurn . whoseTurnPhase . to (\case
    PhaseTile -> True
    _ -> False))
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
      renderSpot :: HasAppContext r => (Int, Int) -> LiveView r Message
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
                Nothing -> when isPhasePlaceTile $ do
                  turn <- view gameWhoseTurn
                  me <- asks myPlayerIndex
                  mayPlaceCurrTile <- do
                    let (WhoseTurn player phase) = turn
                    if player /= me then pure Nothing else
                      case phase of
                        PhaseTile ->
                          case mayCurrTile <&> (\t -> (t, canPlace t (neighborhood loc))) of
                            Just (currTile, True) ->
                              Just
                                <$> addActionBinding
                                  "click"
                                  (\_ -> PlaceTile loc)
                            _ -> pure Nothing
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
    [ classes_ ("board" : ["is-phase-place-tile" | isPhasePlaceTile]),
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
    Just (mplace, PlayerIndex i) -> do
      let gridArea = case mplace of
            PlaceMonastery -> "2 / 2"
            PlaceAbbot -> "2 / 2"
            PlaceSide L -> "2 / 1"
            PlaceSide R -> "2 / 3"
            PlaceSide U -> "1 / 2"
            PlaceSide D -> "3 / 2"
          color = ["red", "blue", "yellow", "green", "purple", "black"] !! fromIntegral i
          isAbbot = case mplace of PlaceAbbot -> True; _ -> False
      div_ [class_ "meeple-container svg-container"] $ div_ [style_ [txt|
                          grid-area: $gridArea;
                          text-align: center;
                          fill: $color;
                          |]] (if isAbbot then abbotSvg else meepleSvg)

meepleSvg :: Monad m => HtmlT m ()
meepleSvg = toHtmlRaw [txt|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">
  <!-- originally by Delapouite (https://game-icons.net/1x1/delapouite/meeple.html) under CC BY 3.0, but modified to remove fixed fill/size -->
  <g class="" transform="translate(0,0)"><path d="M256 54.99c-27 0-46.418 14.287-57.633 32.23-10.03 16.047-14.203 34.66-15.017 50.962-30.608 15.135-64.515 30.394-91.815 45.994-14.32 8.183-26.805 16.414-36.203 25.26C45.934 218.28 39 228.24 39 239.99c0 5 2.44 9.075 5.19 12.065 2.754 2.99 6.054 5.312 9.812 7.48 7.515 4.336 16.99 7.95 27.412 11.076 15.483 4.646 32.823 8.1 47.9 9.577-14.996 25.84-34.953 49.574-52.447 72.315C56.65 378.785 39 403.99 39 431.99c0 4-.044 7.123.31 10.26.355 3.137 1.256 7.053 4.41 10.156 3.155 3.104 7.017 3.938 10.163 4.28 3.146.345 6.315.304 10.38.304h111.542c8.097 0 14.026.492 20.125-3.43 6.1-3.92 8.324-9.275 12.67-17.275l.088-.16.08-.166s9.723-19.77 21.324-39.388c5.8-9.808 12.097-19.576 17.574-26.498 2.74-3.46 5.304-6.204 7.15-7.754.564-.472.82-.56 1.184-.76.363.2.62.288 1.184.76 1.846 1.55 4.41 4.294 7.15 7.754 5.477 6.922 11.774 16.69 17.574 26.498 11.6 19.618 21.324 39.387 21.324 39.387l.08.165.088.16c4.346 8 6.55 13.323 12.61 17.254 6.058 3.93 11.974 3.45 19.957 3.45H448c4 0 7.12.043 10.244-.304 3.123-.347 6.998-1.21 10.12-4.332 3.12-3.122 3.984-6.997 4.33-10.12.348-3.122.306-6.244.306-10.244 0-28-17.65-53.205-37.867-79.488-17.493-22.74-37.45-46.474-52.447-72.315 15.077-1.478 32.417-4.93 47.9-9.576 10.422-3.125 19.897-6.74 27.412-11.075 3.758-2.168 7.058-4.49 9.81-7.48 2.753-2.99 5.192-7.065 5.192-12.065 0-11.75-6.934-21.71-16.332-30.554-9.398-8.846-21.883-17.077-36.203-25.26-27.3-15.6-61.207-30.86-91.815-45.994-.814-16.3-4.988-34.915-15.017-50.96C302.418 69.276 283 54.99 256 54.99z" fill-opacity="1"></path></g></svg>|]

abbotSvg :: Monad m => HtmlT m ()
abbotSvg = toHtmlRaw [txt|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">
  <!-- originally by Delapouite (https://game-icons.net/1x1/delapouite/abbot-meeple.html) under CC BY 3.0, but modified to remove fixed fill/size -->
  <g class="" transform="translate(0,0)" style=""><path d="M256 16.1l-82.7 82.72 35 69.98a66.18 66.18 0 0 0-18.6 45.8 66.18 66.18 0 0 0 10.4 35.3c-42.8 5.3-126.12 21.2-126.12 47.5 0 16.8 30.02 37.8 68.12 54.5L73.98 462.8c0 33.1 33.02 33.1 66.22 33.1h231.5c33.1 0 66.3 0 66.3-33.1l-68.2-110.9c38.1-16.7 68.2-37.7 68.2-54.5 0-26.3-83.5-42.2-126.1-47.5a66.18 66.18 0 0 0 10.2-35.3 66.18 66.18 0 0 0-18.4-45.8l35-69.98z" fill-opacity="1"></path></g></svg>|]