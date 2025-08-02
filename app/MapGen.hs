{-# LANGUAGE RecordWildCards #-}

module MapGen (randomMatrix) where

import Types
import Utils
import System.Random (StdGen, randomR, SplitGen (splitGen))

isLava :: [[Tile]] -> Maybe (Int, Int) -> Bool
isLava [] Nothing = False
isLava [] _       = False
isLava _ Nothing  = False
isLava (r : _) (Just (x, _)) = r !! x == Lava

isLavaBehind :: [Tile] -> Bool
isLavaBehind [] = False
isLavaBehind (t:_) = t == Lava

isLavaAdjacent :: [[Tile]] -> [Tile] -> Int -> Int -> Bool
isLavaAdjacent prev behind r c =
    isLavaBehind behind || any (isLava prev) [move (c-1, r-1), move (c, r-1), move (c+1, r-1)]

selectTreasure :: Int -> Int -> Bool
selectTreasure val t
    | val <= t  = True
    | otherwise = False

selectTile :: StdGen -> Int -> Probabilities -> Bool -> Tile
selectTile gen val probs hasAdjacentLava =
    let Probabilities{..} = probs
    in if val <= w then Water
       else if val <= w + p then Portal
       else if hasAdjacentLava
           then if val <= w + p + ll then Lava
           else Desert (selectTreasure  (fst $ randomR (1, 100) gen) t)
       else if val <= w + p + l then Lava
       else Desert (selectTreasure  (fst $ randomR (1, 100) gen) t)

randomTiles :: StdGen -> Int -> Int -> Probabilities -> [[Tile]] -> [Tile] -> [Tile]
randomTiles gen r c probs prev behind =
    let (val, gen') = randomR (1, 100) gen
        lava = isLavaAdjacent prev behind r c
        tile = selectTile gen' val probs lava
    in tile : randomTiles gen' r (inc c) probs prev (tile : behind)

randomMatrix :: StdGen -> Probabilities -> [[Tile]]
randomMatrix gen probs = go [] (iterate (snd . splitGen) gen) 0
  where
    go acc (g:gs) row =
        let newRow = randomTiles g row 0 probs acc []
        in newRow : go (newRow:acc) gs (row + 1)
    go _ [] _ = []
