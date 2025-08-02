{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GameLogic where

import WormLogic
import Types
import GameClass
import Utils
import MapGen
import System.Random (mkStdGen)



instance GameState DesertGameState where
    isFinalState DesertGameState{..} =
        gameOver ||
        health player == 0 ||
        thirst player == 0 ||
        isGone player ||
        checkWormCollision DesertGameState{..}

    isWinningState DesertGameState{..} = isGone player

    nextState :: DesertGameState -> Command -> Maybe DesertGameState
    nextState state cmd 
        | isFinalState state = Nothing
        | otherwise = handleMovement state cmd

-- Separated player movement logic:
handleMovement :: DesertGameState -> Command -> Maybe DesertGameState
handleMovement state@DesertGameState{..} cmd =
    let (x, y) = position player
        newPos = case cmd of
            "W" -> move (x, y - 1)
            "S" -> move (x, y + 1)
            "A" -> move (x - 1, y)
            "D" -> move (x + 1, y)
            [c] | c `elem` "wasd" -> case c of
                'w' -> move (x, y - 1)
                's' -> move (x, y + 1)
                'a' -> move (x - 1, y)
                'd' -> move (x + 1, y)
                _   -> Nothing
            _ -> Nothing
    in
        case newPos of
            Just (nx, ny) ->
                let tile = (mapState !! ny) !! nx
                    newHealth = if tile == Lava then 0 else health player
                    newThirst = if tile == Water then m config else max 0 (thirst player - 1)
                    newTreasures = if tile == Desert True then treasures player + 1 else treasures player
                    escaped = tile == Portal

                    updatedTreasuresCollected = if tile == Desert True then (nx, ny) : treasuresCollected else treasuresCollected

                    newRevealedTiles = updateRevealedTiles revealedTiles (nx, ny)

                    updatedMapState = case tile of
                        Desert True -> updateTile (nx, ny) (Desert False) mapState
                        _        -> mapState

                    updatedPlayer = Player (nx, ny) newHealth newThirst newTreasures escaped
                    newState = state { player = updatedPlayer, mapState=updatedMapState, treasuresCollected=updatedTreasuresCollected, revealedTiles=newRevealedTiles, explprationStepCounter = explprationStepCounter + 1 }

                in if checkWormCollision newState
                   then Just newState { gameOver = True }
                   else Just newState
            Nothing -> Nothing

    where

        updateRevealedTiles :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
        updateRevealedTiles revealed (x, y) =
            let newRevealed = tilesInRadius (x, y) (s config)
            in  foldr (\tile acc -> if tile `elem` revealed then acc else tile : acc) revealed newRevealed



tilesInRadius :: (Int, Int) -> Int -> [(Int, Int)]
tilesInRadius (cx, cy) r =
    [(x, y) |
       x <- [cx - r .. cx + r], x >= 0, y <- [cy - r .. cy + r], y >= 0]

instance TerminalGame DesertGameState DesertGameConfig where
    initialState :: DesertGameConfig -> Either String DesertGameState
    initialState config@DesertGameConfig{..}
        | isValidProb probs =
            let matrix = randomMatrix (mkStdGen g) probs
            in  Right $ DesertGameState
                    (Player (0, 0) 2 m 0 False)
                    []
                    matrix
                    config
                    (tilesInRadius (0,0) s)
                    []
                    False
                    0
        | otherwise = Left "Invalid configuration: probabilities must be valid"


    loadStateFrom :: DesertGameConfig -> Player -> [(Int, Int)] -> [(Int, Int)] -> [Worm] -> DesertGameState
    loadStateFrom config'@DesertGameConfig{..} player' revealedTiles' treasures worms' =
        let
            matrix = foldl (\m' (x', y') -> updateTile (x', y') (Desert False) m') (randomMatrix (mkStdGen g) probs) treasures
        in
             (DesertGameState {
            player = player',
            worms = worms',
            mapState = matrix,
            config = config',
            revealedTiles = revealedTiles',
            treasuresCollected = treasures,
            gameOver = False,
            explprationStepCounter = 0
        })


updateTile :: (Int, Int) -> Tile -> [[Tile]] -> [[Tile]]
updateTile (x, y) newTile matrix =
    let (beforeRows, targetRow:afterRows) = splitAt y matrix
        (beforeCols, _:afterCols) = splitAt x targetRow
        updatedRow = beforeCols ++ [newTile] ++ afterCols
    in beforeRows ++ [updatedRow] ++ afterRows


takeSubMatrix :: Player -> [[Tile]] -> [[Tile]]
takeSubMatrix (Player (x, y) _ _ _ _) mapState =
    let half = sizeOfMap `div` 2
        startX = max 0 (x - half)
        startY = max 0 (y - half)
        subRows = take sizeOfMap $ drop startY mapState
    in map (take sizeOfMap . drop startX) subRows