{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module GameSaver where
import Types
import Data.List (intercalate)


saveGame :: DesertGameState -> IO ()
saveGame DesertGameState{..} = do
    writeFile "saved_game.txt" content
    putStrLn "Game saved to saved_game.txt"
  where
    content = 
        "position([" ++ show (fst $ position player) ++ " , " ++ show (snd $ position player) ++ "])\n" ++
        "supply(" ++ show (thirst player) ++ ")\n"
        ++ addAllRevealedTiles
        ++ concat addCollectedTreasures
        ++ addAllEmergingWorms
        ++ addAllDisappearingWorms
        ++ "s(" ++ show (s config) ++ ")\n"
        ++ "m(" ++ show (m config) ++ ")\n"
        ++ "g(" ++ show (g config) ++ ")\n"
        ++ "t(" ++ show (t $ probs config) ++ ")\n"
        ++ "w(" ++ show (w $ probs config) ++ ")\n"
        ++ "p(" ++ show (p $ probs config) ++ ")\n"
        ++ "l(" ++ show (l $ probs config) ++ ")\n"
        ++ "ll(" ++ show (ll $ probs config) ++ ")\n"
        ++ "x(" ++ show (x config) ++ ")\n"
        ++ "y(" ++ show (y config) ++ ")\n"
    
    addAllRevealedTiles = concatMap (\(x, y) -> "revealed([" ++ show x ++ " , " ++ show y ++ "])\n") revealedTiles
    addCollectedTreasures = map (\(x, y) -> "collected([" ++ show x ++" , " ++ show y ++ "])\n") treasuresCollected
    addAllEmergingWorms = concatMap (\worm -> "emerging(" ++ getWormPositions worm ++ ")\n") $ filter ((== Emerging) . phase) worms
    addAllDisappearingWorms = concatMap (\worm -> "disappearing(" ++ getWormPositions worm ++ ")\n") $ filter ((== Disappearing) . phase) worms
    getWormPositions worm = intercalate ", " $ map (\(WormSegment (x, y) _) -> "[" ++ show x ++ " , " ++ show y ++ "]") (body worm)  