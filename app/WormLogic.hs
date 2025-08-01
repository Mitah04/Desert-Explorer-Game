{-# LANGUAGE RecordWildCards #-}
module WormLogic where

import Types
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Data.Maybe (mapMaybe)
import Control.Monad (unless)

spawnWorms :: TVar DesertGameState -> Int -> IO ()
spawnWorms stateVar spawnDelayMicros =do
    gen <- newTVarIO =<< newStdGen
    let spawnLoop = do
            gameState <- readTVarIO stateVar
            unless (gameOver gameState) $ do
                maybeWorm <- spawnSingleWorm stateVar gen
                case maybeWorm of
                    Just worm -> do
                        _ <- forkIO $ wormLifeCycle stateVar worm
                        return ()
                    Nothing -> return ()

                threadDelay spawnDelayMicros
                spawnLoop
    spawnLoop

spawnSingleWorm :: TVar DesertGameState -> TVar StdGen -> IO (Maybe Worm)
spawnSingleWorm stateVar genVar = atomically $ do
    game <- readTVar stateVar
    gen <- readTVar genVar

    let wormLength = x (config game)
        maybeSpawnPos = findValidSpawnPosition game wormLength gen

    case maybeSpawnPos of
        Just spawnPositions -> do
            let wormSegments = case spawnPositions of
                    [] -> []
                    (headPos:tailPositions) ->
                        WormSegment headPos True : map (`WormSegment` False) tailPositions

                existingIds = map wormId (worms game)
                newWormId = if null existingIds
                           then 1
                           else maximum existingIds + 1

                newWorm = Worm newWormId wormSegments Emerging (explprationStepCounter game)

            modifyTVar stateVar $ \g -> g { worms = newWorm : worms g }

            let (_, gen') = splitGen gen
            writeTVar genVar gen'

            return (Just newWorm)
        Nothing -> return Nothing

findValidSpawnPosition :: DesertGameState -> Int -> StdGen -> Maybe [(Int, Int)]
findValidSpawnPosition DesertGameState{..} wormLength gen =
    let (px, py) = position player
        searchRadius = 10

        candidatePositions = [(x, y) |
            x <- [max 0 (px - searchRadius)..px + searchRadius],
            y <- [max 0 (py - searchRadius)..py + searchRadius],
            validPosition (x, y)]

        validSequences = mapMaybe (generateWormSequence wormLength) candidatePositions

    in case validSequences of
        [] -> Nothing
        _  -> Just (validSequences !!  fst (randomR (0, length validSequences - 1) gen))
  where
    validPosition (x, y) =
        x >= 0 && y >= 0 &&
        case (mapState !! y) !! x of
            Desert False -> True
            _ -> False

    generateWormSequence :: Int -> (Int, Int) -> Maybe [(Int, Int)]
    generateWormSequence len (x, y) =
        let directions = [[(x+i, y) | i <- [0..len-1]],
                         [(x-i, y) | i <- [0..len-1]],
                         [(x, y+i) | i <- [0..len-1]],
                         [(x, y-i) | i <- [0..len-1]]]

            validSequences = filter (all isValidForWorm) directions

        in case validSequences of
            [] -> Nothing
            (seq':_) -> Just seq'

    isValidForWorm pos =
        validPosition pos &&
        pos /= position player &&
        not (any (elem pos . map segmentPosition . body) worms)



wormLifeCycle :: TVar DesertGameState -> Worm -> IO ()
wormLifeCycle stateVar initialWorm = do
    loop initialWorm
  where
    loop currentWorm = do
        gameState <- readTVarIO stateVar
        if gameOver gameState
            then return ()
            else do
                let
                    currentStep = explprationStepCounter gameState
                    wormStep = steps currentWorm

                if currentStep > wormStep then do

                    nextWorm <- case phase currentWorm of
                        Emerging -> handleEmergingPhase stateVar currentWorm
                        Disappearing -> handleDisappearingPhase stateVar currentWorm
                        Spawning -> return (Just currentWorm)

                    case nextWorm of
                        Just worm -> do
                            threadDelay 100000
                            loop worm
                        Nothing -> do
                            putStrLn $ "Worm " ++ show (wormId currentWorm) ++ " lifecycle ended"
                            removeWorm stateVar (wormId currentWorm)
                else do
                    threadDelay 100000
                    loop currentWorm


handleEmergingPhase :: TVar DesertGameState -> Worm -> IO (Maybe Worm)
handleEmergingPhase stateVar worm = atomically $ do
    game <- readTVar stateVar

    case body worm of
        [] -> return Nothing
        (headSeg:tailSegs) -> do
            let headPos = segmentPosition headSeg
                possibleMoves = getAdjacentPositions headPos
                validMoves = filter (`isValidMove` game) possibleMoves

            case validMoves of
                [] -> do
                    let disappearingWorm = worm { phase = Disappearing, steps = steps worm + 1 }
                    updateWormInGame stateVar worm disappearingWorm
                    return (Just disappearingWorm)
                (newHeadPos:_) -> do
                    let newHead = WormSegment newHeadPos True
                        revealedTail = case tailSegs of
                            [] -> []
                            (nextSeg:restSegs) -> WormSegment headPos True : nextSeg : restSegs
                        newBody = case revealedTail of
                            [] -> [newHead]  
                            _  -> newHead : init revealedTail
                        movedWorm = worm { body = newBody, steps = steps worm + 1 }

                    updateWormInGame stateVar worm movedWorm
                    return (Just movedWorm)


handleDisappearingPhase :: TVar DesertGameState -> Worm -> IO (Maybe Worm)
handleDisappearingPhase stateVar worm = atomically $ do
    case body worm of
        [] -> return Nothing
        [_] -> do
            return Nothing
        segments -> do
            let newBody = init segments
                disappearingWorm = worm { body = newBody, steps = steps worm + 1 }

            updateWormInGame stateVar worm disappearingWorm
            return (Just disappearingWorm)

getAdjacentPositions :: (Int, Int) -> [(Int, Int)]
getAdjacentPositions (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

isValidMove :: (Int, Int) -> DesertGameState -> Bool
isValidMove pos@(x, y) DesertGameState{..} =
    x >= 0 && y >= 0 &&
    case (mapState !! y) !! x of
        Desert False -> True 
        _ -> False
    && not (any (elem pos . map segmentPosition . body) worms)

updateWormInGame :: TVar DesertGameState -> Worm -> Worm -> STM ()
updateWormInGame stateVar oldWorm newWorm = do
    game <- readTVar stateVar
    let updatedWorms = newWorm : filter ((/= wormId oldWorm) . wormId) (worms game)
    writeTVar stateVar (game { worms = updatedWorms })

removeWorm :: TVar DesertGameState -> Int -> IO ()
removeWorm stateVar wormIdToRemove = atomically $ do
    game <- readTVar stateVar
    let updatedWorms = filter ((/= wormIdToRemove) . wormId) (worms game)
    writeTVar stateVar (game { worms = updatedWorms })

checkWormCollision :: DesertGameState -> Bool
checkWormCollision DesertGameState{..} =
    let playerPos = position player
        allWormPositions = concatMap (map segmentPosition . filter isVisible . body) worms
    in playerPos `elem` allWormPositions