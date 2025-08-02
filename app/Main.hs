module Main where

import System.Environment (getArgs)
import Gui (runGameGui, runGameGuiFromState)
import Text.Parsec.String (parseFromFile)
import GameLoader(gameParser)
import Utils (isValidProb)
import Types (DesertGameConfig(..), Probabilities(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--load", saveFile] -> do
            result <- parseFromFile gameParser saveFile
            case result of
                Left err -> print err
                Right game -> runGameGuiFromState game
        otherArgs -> case parseArgs otherArgs of
            Right config -> runGameGui config
            Left err -> putStrLn $ "Error: " ++ err

parseArgs :: [String] -> Either String DesertGameConfig
parseArgs [s', m', g', t', w', p', l', ll', x', y'] = do
    let probs' = Probabilities (read t') (read w') (read p') (read l') (read ll')
    if isValidProb probs'
        then Right $ DesertGameConfig (read s') (read m') (read g') (read x') (read y') probs'
        else Left "Invalid probabilities configuration"
parseArgs _ = Left "Usage: ./desert-agame SIGHT THIRST SEED T W P L LL X Y"