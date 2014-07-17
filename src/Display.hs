module Display (drawWorld) where

--------------------------------------------------
import           Data.List           (intercalate)
import           System.Console.ANSI

import           Coord
import           Level
import           Types
--------------------------------------------------

-- | Feature is a Draw type which is either Blank,
-- a new line, or a representation for a game tile.
data Feature = Blank
             | Newline
             | Feature Tile

-- | Convert a Coordinate in the World to a
-- drawable Feature.
coordToFeature :: Coord -> World -> Feature
coordToFeature coord (World _ player lvl _)
  | player == coord   = (Feature Player)
  | isWall coord lvl  = (Feature Wall)
  | isBlock coord lvl = (Feature Block) -- Priority over Goal.
  | isGoal coord lvl  = (Feature Goal)
  | otherwise         = Blank

-- | Draw Feature using character glyph.
drawFeature :: Feature -> IO ()
drawFeature (Feature Player) = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putChar '☺'
drawFeature (Feature Wall) = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar '█'
drawFeature (Feature Block) = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Green ]
  putChar '⛃'
drawFeature (Feature Goal) = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putChar '░'
drawFeature Newline = do
  putChar '\n'
drawFeature _ = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar ' '

-- | Draw World state.
drawWorld :: World -> IO ()
drawWorld world = do
  setCursorPosition 0 0
  mapM_ drawFeature features
  where
    lvl = wLevel world
    Coord x' y' = lMax lvl
    features = intercalate [Newline]
               [[coordToFeature (Coord x y) world
                | x <- [0..x']]
                | y <- [0..y']]
