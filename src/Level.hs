module Level ( readLevel
             , isWall
             , isGoal
             , isBlock
             , pushBlock
             , isComplete
             ) where

--------------------------------------------------
import qualified Data.Map as M
import qualified Data.Set as S

import           Coord
import           Types
--------------------------------------------------

-- | Read Level information from a file.
readLevel :: String -> IO Level
readLevel filename = do
  map <- readFile filename
  let map' = strsToLevel (lines map)
  return map'

{-|
  Read a String representation of a map and convert
  it to the internal Level structure.

  Key:

   * '#' = Wall

   * '_' = Goal

   * 'O' = Block

   * '@' = Player starting location
-}
strsToLevel :: [String] -> Level
strsToLevel str = foldl populate emptyLevel { lMax = maxXY } asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords = [[(Coord x y) | x <- [0..]] | y <- [0..]]
    maxX = maximum . map (x . fst) $ asciiMap
    maxY = maximum . map (y . fst) $ asciiMap
    maxXY = Coord maxX maxY
    populate lvl (coord, tile) =
      case tile of
        '#' -> lvl { lTiles  = M.insert coord Wall         t}
        '_' -> lvl { lTiles  = M.insert coord Goal         t}
        'O' -> lvl { lBlocks = S.insert coord              b}
        '@' -> lvl { lStart  = coord }
        _ ->   lvl
        where
          t = lTiles lvl
          b = lBlocks lvl

-- | True if (x, y) is a Wall tile, else False.
isWall coord lvl = case M.lookup coord (lTiles lvl) of
  Just Wall -> True
  _         -> False

-- | True if (x, y) is a Goal tile, else False.
isGoal coord lvl = case M.lookup coord (lTiles lvl) of
  Just Goal -> True
  _         -> False

-- | True if (x, y) is a Block tile, else False.
isBlock coord lvl = S.member coord (lBlocks lvl)

{-|
  True when the level is complete.
  A level is complete when all Goal tiles
  contain blocks.
-}
isComplete :: Level -> Bool
isComplete lvl@(Level _ _ _ _ tiles blocks) =
  all (`S.member` blocks) goals
  where
    coords = M.keys tiles
    goals = filter (\x -> isGoal x lvl) coords

moveBlock :: Level -> Coord -> Coord -> Level
moveBlock lvl pos newPos
  | S.member pos blocks = lvl { lBlocks = blocks' }
  | otherwise           = lvl
  where
    blocks = lBlocks lvl
    blocks' = S.insert newPos $ S.delete pos blocks

-- | Push a block at coord 1 position in the given
--   direction.
pushBlock :: Level -> Coord -> Direction -> Level
pushBlock lvl pos dir
  | S.member pos blocks = moveBlock lvl' pos newPos
  | otherwise           = lvl
  where
    blocks = lBlocks lvl
    newPos = pos + dirToCoord dir
    lvl'   = pushBlock lvl newPos dir
