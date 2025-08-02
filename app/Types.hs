module Types where


type Command = String

type ExplorationStepCounter = Int


data Tile = Desert {isTreasure :: Bool} | Water | Lava | Portal deriving (Show, Eq)

data Player = Player {
    position :: (Int, Int),
    health :: Int,
    thirst :: Int,
    treasures :: Int,
    isGone :: Bool
} deriving Show

data Probabilities = Probabilities {
    t :: Int, w :: Int, p :: Int, l :: Int, ll :: Int
} deriving Show

data DesertGameConfig = DesertGameConfig {
    s :: Int,    -- line of sight
    m :: Int,    -- water capacity
    g :: Int,    -- seed
    x :: Int,    -- worm length
    y :: Int,    -- worm spawn rate (in seconds)
    probs :: Probabilities
} deriving Show

data DesertGameState = DesertGameState {
    player :: Player, 
    worms :: [Worm],
    mapState :: [[Tile]], 
    config :: DesertGameConfig,
    revealedTiles :: [(Int, Int)], 
    treasuresCollected :: [(Int, Int)],
    gameOver :: Bool,
    explprationStepCounter :: ExplorationStepCounter
}

data Worm = Worm
  { wormId :: Int
  , body :: [WormSegment]
  , phase :: WormPhase
  , steps :: Int
  } deriving (Show, Eq)

data WormSegment = WormSegment
  { segmentPosition :: (Int, Int)
  , isVisible :: Bool
  } deriving (Show, Eq)

data WormPhase = Spawning | Emerging | Disappearing deriving (Show, Eq)
