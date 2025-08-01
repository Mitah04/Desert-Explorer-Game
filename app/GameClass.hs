{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module GameClass where

import Types

class GameState s where
    nextState :: s -> Command -> Maybe s
    isFinalState :: s -> Bool
    isWinningState :: s -> Bool

class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> Either String s
    loadStateFrom :: c -> Player -> [(Int, Int)] -> [(Int, Int)] -> [Worm] -> s




