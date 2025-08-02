{-# LANGUAGE RecordWildCards #-}

module Utils where

import Types
import Graphics.Gloss.Interface.Pure.Game


keyToCommand::Key -> Command
keyToCommand (Char c) = [c]
keyToCommand _ = "_"



isValidProb :: Probabilities -> Bool
isValidProb Probabilities{..} =
    w + p + l <= 100 && w + p + ll <= 100

move :: (Int, Int) -> Maybe (Int, Int)
move (x, y)
    | x < 0 || y < 0 = Nothing
    | otherwise      = Just (x, y)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

inc :: Num a => a -> a
inc x = x + 1

sizeOfMap :: Int
sizeOfMap = 20