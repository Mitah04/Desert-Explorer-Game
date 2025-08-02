{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Gui where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game (playIO)
import Types
import Control.Monad (unless, when)
import GameLogic
import WormLogic
import Utils
import GameClass (GameState(nextState), initialState, isFinalState, isWinningState)
import Control.Concurrent
import Control.Concurrent.STM
import GameSaver
import Data.Foldable 
import BFS (bfs)

runGameGui :: DesertGameConfig -> IO ()
runGameGui config = case initialState config of
    Left err -> error err
    Right initial -> do
        stateVar <- newTVarIO initial
        -- Start worm spawning with configured spawn rate
        let spawnDelayMicros = y config * 1000000  -- Convert seconds to microseconds
        _ <- forkIO $ spawnWorms stateVar spawnDelayMicros
        specs <- getScreenSize
        printGui stateVar specs

runGameGuiFromState :: DesertGameState -> IO ()
runGameGuiFromState state  = do
    stateVar <- newTVarIO state
    -- Start worm spawning with configured spawn rate
    let spawnDelayMicros = y (config state) * 1000000  -- Convert seconds to microseconds
    _ <- forkIO $ spawnWorms stateVar spawnDelayMicros
    mapM_ (forkIO . wormLifeCycle stateVar) (worms state)
    specs <- getScreenSize
    printGui stateVar specs

printGui :: TVar DesertGameState -> (Int, Int) -> IO ()
printGui initialStateTVar (windowWidth, windowHeight) =
    playIO window background fps initialStateTVar render handleInput updateState
  where
    window     = InWindow "Desert Game" (windowWidth, windowHeight) (100, 100)
    background = black
    fps        = 60

    render :: TVar DesertGameState -> IO Picture
    render stateVar = do
        state@DesertGameState {player, worms, mapState} <- readTVarIO stateVar
        return $
            if isFinalState state
            then displayEndScreen state
            else pictures [
                renderMap player (takeSubMatrix player mapState) worms (revealedTiles state),
                renderUI state
            ]

    handleInput :: Event -> TVar DesertGameState -> IO (TVar DesertGameState)
    handleInput (EventKey (Char '\DC3') Down _ _) stateVar = do
        st <- readTVarIO stateVar
        saveGame st
        putStrLn "Game saved."
        return stateVar

    handleInput event stateVar = do
        atomically $ do
            st <- readTVar stateVar
            unless (isFinalState st) $
                forM_
              (handleInputMaybe event st) (writeTVar stateVar)
        return stateVar

    handleInputMaybe :: Event -> DesertGameState -> Maybe DesertGameState
    handleInputMaybe (EventKey key Down _ _) state = nextState state (keyToCommand key)
    handleInputMaybe _ _ = Nothing

    updateState :: Float -> TVar DesertGameState -> IO (TVar DesertGameState)
    updateState _ stateVar = do
        -- Only update game over state when needed
        atomically $ do
            state <- readTVar stateVar
            when (isFinalState state && not (gameOver state)) $
                writeTVar stateVar (state { gameOver = True })
        return stateVar

    displayEndScreen :: DesertGameState -> Picture
    displayEndScreen state =
        let treasuresCollected = treasures (player state)
            message
                | isWinningState state = "You Won! You reached the portal!"
                | checkWormCollision state
                = "Game Over! You were caught by a worm!"
                | health (player state) == 0 = "Game Over! You died from lava!"
                | otherwise = "Game Over! You died of thirst!"
            treasuresMessage = "Treasures Collected: " ++ show treasuresCollected
        in pictures [
            translate (-300) 50 $ scale 0.5 0.5 $ color white $ text message,
            translate (-300) (-50) $ scale 0.5 0.5 $ color yellow $ text treasuresMessage
        ]

    renderMap :: Player -> [[Tile]] -> [Worm] -> [(Int, Int)] -> Picture
    renderMap (Player (px, py) _ _ _ _) tileMatrix worms revealedTiles =
        pictures $
            [ tileImage tile (x, y) (rows, cols) (mapWidth, mapHeight) ((x + startX, y + startY) `elem` revealedTiles)
            | (row, y) <- zip tileMatrix [0..]
            , (tile, x) <- zip row [0..]
            ] ++
            [playerImage (px - startX, py - startY) (rows, cols) (mapWidth, mapHeight)] ++
            [renderWorms worms (rows, cols) (mapWidth, mapHeight) (startX, startY) revealedTiles]
        where
            startX = max 0 (px - (sizeOfMap `div` 2))
            startY = max 0 (py - (sizeOfMap `div` 2))
            rows = length tileMatrix
            cols = if null tileMatrix then 0 else length (head tileMatrix)
            mapWidth  = floor (fromIntegral windowWidth * (0.7 :: Double))
            mapHeight = floor (fromIntegral windowHeight * (0.7 :: Double))


    renderUI :: DesertGameState -> Picture
    renderUI state = pictures [
        addPlayerInfo state,
        addDistanceInfo state,
        addWormInfo state
        ]

    addPlayerInfo :: DesertGameState -> Picture
    addPlayerInfo state =
        let p = player state
        in pictures [
            translate (fromIntegral windowWidth / 2 - 1000) 300 $
                scale 0.4 0.4 $ color white $ text ("Health: " ++ show (health p)),
            translate (fromIntegral windowWidth / 2 - 1000) 250 $
                scale 0.4 0.4 $ color cyan $ text ("Thirst: " ++ show (thirst p)),
            translate (fromIntegral windowWidth / 2 - 1000) 200 $
                scale 0.4 0.4 $ color yellow $ text ("Treasures: " ++ show (treasures p))
        ]

    addDistanceInfo :: DesertGameState -> Picture
    addDistanceInfo state =
        let p = player state
            distPortal = bfs Portal [] (mapState state) p []
            distWater = bfs Water [] (mapState state) p []
            distTreasure = bfs (Desert True) [] (mapState state) p []
        in pictures [
            translate (fromIntegral windowWidth / 2 - 1000) 100 $
                scale 0.4 0.4 $ color green $ text ("Portal: " ++ if distPortal == maxBound then "∞" else show distPortal),
            translate (fromIntegral windowWidth / 2 - 1000) 50 $
                scale 0.4 0.4 $ color blue $ text ("Water: " ++ if distWater == maxBound then "∞" else show distWater),
            translate (fromIntegral windowWidth / 2 - 1000) 0 $
                scale 0.4 0.4 $ color orange $ text ("Treasure: " ++ if distTreasure == maxBound then "∞" else show distTreasure)
        ]

    addWormInfo :: DesertGameState -> Picture
    addWormInfo state =
        translate (fromIntegral windowWidth / 2 - 1000) (-50) $
            scale 0.4 0.4 $ color red $ text ("Worms: " ++ show (length (worms state)))


tileImage :: Tile -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool -> Picture
tileImage tile (x, y) (rows, cols) (mapWidth, mapHeight) isRevealed =
    let cellSize = min (fromIntegral mapWidth / fromIntegral cols)
                       (fromIntegral mapHeight / fromIntegral rows)
        nx = fromIntegral x * cellSize - fromIntegral mapWidth / 2 + cellSize / 2
        ny = fromIntegral (rows - y - 1) * cellSize - fromIntegral mapHeight / 2 + cellSize / 2
    in translate nx ny $ color (if isRevealed then tileColor tile else greyN 0.5) $ rectangleSolid cellSize cellSize

tileColor :: Tile -> Color
tileColor (Desert True)  = orange
tileColor (Desert False) = yellow
tileColor Water          = blue
tileColor Portal         = green
tileColor Lava           = red


playerImage :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Picture
playerImage (x, y) (rows, cols) (mapWidth, mapHeight) =
    let cellSize = min (fromIntegral mapWidth / fromIntegral cols)
                       (fromIntegral mapHeight / fromIntegral rows)
        nx = fromIntegral x * cellSize - fromIntegral mapWidth / 2 + cellSize / 2
        ny = fromIntegral (rows - y - 1) * cellSize - fromIntegral mapHeight / 2 + cellSize / 2
    in translate nx ny $ color magenta $ circleSolid (cellSize / 2.5)

renderWorms :: [Worm] -> (Int, Int) -> (Int, Int) -> (Int, Int)-> [(Int, Int)] -> Picture
renderWorms worms (rows, cols) (mapWidth, mapHeight) (startX, startY) revealedTiles=
  let
    cellSize = min (fromIntegral mapWidth / fromIntegral cols)
                   (fromIntegral mapHeight / fromIntegral rows)

    toScreen (x, y) =
      let nx = fromIntegral (x - startX) * cellSize
               - fromIntegral mapWidth  / 2 + cellSize / 2
          ny = fromIntegral (rows - (y - startY) - 1) * cellSize
               - fromIntegral mapHeight / 2 + cellSize / 2
      in (nx, ny)

    inBounds (x, y) =
      x >= startX && x < startX + cols &&
      y >= startY && y < startY + rows

    visibleWormSegments = concatMap body worms
    visiblePositions = [(segmentPosition seg, isVisible seg) | seg <- visibleWormSegments]

  in pictures
      [ translate nx ny $ color red $ circleSolid (cellSize / 3)
      | ((x, y), visible) <- visiblePositions,
        inBounds (x, y)
      , ((x + startX, y + startY) `elem` revealedTiles) && visible 
      , let (nx, ny) = toScreen (x, y)
      ]