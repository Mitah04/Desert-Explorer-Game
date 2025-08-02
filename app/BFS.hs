module BFS where


import Types
import Utils (move)
import Data.List (foldl')

-- Fill the queue with neighboring positions not yet visited
fillQueue :: [(Int, Int)] -> (Int, (Int, Int)) -> [(Int, Int)] -> [(Int, (Int, Int))]
fillQueue directions (dist, (lx, ly)) seen =
    foldl' (\acc (dx, dy) ->
        case move (lx + dx, ly + dy) of
            Just (nx, ny) ->
                if (nx, ny) `elem` seen
                    then acc
                    else acc ++ [(dist + 1, (nx, ny))]
            Nothing -> acc
    ) [] directions

-- Entry point to BFS for a specific tile type from the player's position
bfs :: Tile -> [(Int, (Int, Int))] -> [[Tile]] -> Player -> [(Int, Int)] -> Int
bfs desiredTile [] localMapState (Player pos _ _ _ _) [] =
    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        queue = fillQueue directions (0, pos) []
    in bfs desiredTile queue localMapState (Player pos 0 0 0 False) []

bfs _ [] _ _ _ = maxBound  -- No tile found

bfs desiredTile ((d, (lx, ly)) : q) localMapState localPlayer seen
    | matchesTile desiredTile (localMapState !! ly !! lx) = d
    | localMapState !! ly !! lx /= Lava =
        let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
            nq = fillQueue directions (d, (lx, ly)) seen
        in bfs desiredTile (q ++ nq) localMapState localPlayer (seen ++ [(lx, ly)])
    | otherwise = bfs desiredTile q localMapState localPlayer (seen ++ [(lx, ly)])
  where
    matchesTile :: Tile -> Tile -> Bool
    matchesTile t1 t2 = t1 == t2
