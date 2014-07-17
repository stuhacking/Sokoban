{-| Special Data Types used in the Game |-}
module Types where

--------------------------------------------------
import qualified Data.Set as S
import           Prelude  hiding (Either (..))

import           Coord
--------------------------------------------------

{- Environment -}
data Level = Level { lRank   :: Int
                   , lMoves  :: Int
                   , lStart  :: Coord
                   , lMax    :: Coord
                   , lWalls  :: S.Set Coord
                   , lGoals  :: S.Set Coord
                   , lBlocks :: S.Set Coord
                   }

data World = World { wRank   :: Int
                   , wPlayer :: Coord
                   , wLevel  :: Level
                   , wLevels :: [Level]
                   }

{- Control Data -}
data Input = Dir Direction
           | Exit

data Direction = Up
               | Down
               | Left
               | Right

-- | Convert a Direction to a Coordinate assuming
-- a top left origin coordinate system.
dirToCoord :: Direction -> Coord
dirToCoord d = case d of
  Up    -> Coord 0 (-1)
  Down  -> Coord 0 1
  Left  -> Coord (-1) 0
  Right -> Coord 1 0

-- Define some default data structures for convenience --

-- | Empty Level.
emptyLevel = Level { lRank = 0
                   , lMoves = 0
                   , lStart = Coord 0 0
                   , lMax = Coord 0 0
                   , lWalls = S.empty
                   , lGoals = S.empty
                   , lBlocks = S.empty
                   }

-- | Empty World.
emptyWorld = World { wRank = 0
                , wPlayer = Coord 0 0
                , wLevel = emptyLevel
                , wLevels = [emptyLevel]
                }

