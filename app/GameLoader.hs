module GameLoader where

import Text.Parsec (many1, digit, char, spaces, sepBy, string, between, many, try)
import Text.Parsec.String (Parser)
import Types
import GameClass (TerminalGame(loadStateFrom))
import GameLogic ()

number :: Parser Int
number = read <$> many1 digit

tupleParser :: Parser (Int, Int)
tupleParser = do
  _<-char '['
  spaces
  lx <- number
  spaces
  _ <- char ','
  spaces
  ly <- number
  spaces
  _ <- char ']'
  return (lx, ly)

positionsParser :: Parser [(Int, Int)]
positionsParser = do
  _ <- char '('
  positions <- tupleParser `sepBy` (char ',' >> spaces)
  _ <- char ')'
  return positions

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

revealedTileParser :: Parser (Int, Int)
revealedTileParser = do
    _ <- string "revealed"
    postion <- positionsParser
    _ <- char '\n'
    return (head postion)
    

revealedTilesParser :: Parser [(Int, Int)]
revealedTilesParser = many revealedTileParser


collectedTreasureParser :: Parser (Int, Int)
collectedTreasureParser = do 
    _ <- string "collected"
    treasurePosition <- positionsParser
    _ <- char '\n'
    return (head treasurePosition)


collectedTreasuresParser :: Parser [(Int, Int)]
collectedTreasuresParser = many collectedTreasureParser




emergingWormsParser :: Parser [Worm]
emergingWormsParser = do
    emergingWorms <- many (try (spaces *> emergingWormParserPlaceholder <* spaces))
    return (zipWith (\i w' -> w' { wormId = i }) [0..] emergingWorms)
  where
    emergingWormParserPlaceholder = do
        _ <- string "emerging"
        wormPositions <- positionsParser
        let segments = zipWith WormSegment wormPositions (True : repeat False)
        return $ Worm { wormId = 0, body = segments, phase = Emerging, steps = 0 }


disapearringWormsParser :: Parser [Worm]
disapearringWormsParser = do
    disappearingWorms <- many (try (spaces *> disapearringWormParserPlaceholder <* spaces))
    return (zipWith (\i w' -> w' { wormId = i }) [0..] disappearingWorms)
  where
    disapearringWormParserPlaceholder = do
        _ <- string "disappearing"
        wormPositions <- positionsParser
        let segments = zipWith WormSegment wormPositions (True : repeat False)
        return $ Worm { wormId = 0, body = segments, phase = Disappearing, steps = 0 }



configParser :: Parser DesertGameConfig
configParser = do
    _ <- char 's'
    s' <- parens number
    _ <- char '\n'
    _ <- char 'm'
    m' <- parens number
    _ <- char '\n'
    _ <- char 'g'
    g' <- parens number
    _ <- char '\n'
    probs' <- probsParser
    _ <- char '\n'
    _ <- char 'x'
    x' <- parens number
    _ <- char '\n'
    _ <- char 'y'
    y' <- parens number


    return DesertGameConfig {
        s = s', 
        m = m', 
        g = g',
        probs = probs',
        x = x', 
        y = y'
    }

    where 
        probsParser = do
            _ <- char 't'
            t' <- parens number
            _ <- char '\n'
            _ <- char 'w'
            w' <- parens number
            _ <- char '\n'
            _ <- char 'p'
            p' <- parens number
            _ <- char '\n'
            _ <- char 'l'
            l' <- parens number
            _ <- char '\n'
            _ <- string "ll"
            ll' <- parens number
            return Probabilities {
                t = t',
                w = w', 
                p = p', 
                l = l', 
                ll = ll'
            }





gameParser :: Parser DesertGameState
gameParser = do
    _ <- string "position"
    playerPos <- positionsParser
    _ <- char '\n'
    _ <- string "supply" 
    waterCapacity <- parens number
    _ <- char '\n'
    revealedTiles' <- revealedTilesParser 
    collectedTreasure' <- collectedTreasuresParser
    emerging <- emergingWormsParser
    disapearring <- disapearringWormsParser
    let worms' = emerging ++ disapearring
    config' <- configParser 
    return (loadStateFrom config' (Player (head playerPos) 1 waterCapacity (length collectedTreasure') False)  revealedTiles' collectedTreasure' worms')