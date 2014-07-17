{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main (main) where

--------------------------------------------------
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Prelude             hiding (Either (..))
import           System.Console.ANSI
import           System.Environment
import           System.IO

import           Coord
import           Display
import           Level
import           Types
--------------------------------------------------

{-|
  Read a character and convert it to an
  Input command. Continue until a mapped
  character has been read.
-}
getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'k' -> return (Dir Up)
    'j' -> return (Dir Down)
    'h' -> return (Dir Left)
    'l' -> return (Dir Right)
    'q' -> return Exit
    _   -> getInput

-- | Clear and restore console settings, then exit.
handleWin :: IO ()
handleWin = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Yellow ]
  putStrLn "Congratulations!"

-- | Clear and restore console settings, then exit.
handleExit :: IO ()
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStrLn "Thanks for playing!"

{-|
  A Cell is valid to move into if it is not
  a wall, or it is a block that is also able
  to move in the same direction.
  Bug: Shouldn't be able to push multiple blocks...?
-}
isValid :: Level -> Coord -> Direction -> Bool
isValid lvl player dir
  | isWall target lvl  = False
  | isBlock target lvl = isValid lvl target dir
  | otherwise          = True
  where
    target = player + dirToCoord dir

handleMovement :: World -> Direction -> World
handleMovement world@(World _ player lvl _) dir
  | isBlock newCoord lvl =
    world { wPlayer = newCoord,
            wLevel = pushBlock lvl newCoord dir }
  | otherwise = world { wPlayer = newCoord }
  where
    newCoord = Coord newX newY
    coord@(Coord x' y') = player + dirToCoord dir
    newX              = max 0 (min x' 80)
    newY              = max 0 (min y' 25)

{- |
   Update the world state in the case where
   a Direction command has been entered.
-}
handleDir :: World -> Direction -> IO ()
handleDir w@(World _ player lvl _) input
  | isValid lvl player input = gameLoop $ handleMovement w input
  | otherwise = gameLoop w

-- | Read input, update state, draw world.
gameLoop :: World -> IO ()
gameLoop world@(World _ _ lvl _)
  | isComplete lvl = handleWin
  | otherwise      = do
    drawWorld world
    input <- getInput
    case input of
      Exit    -> handleExit
      Dir dir -> handleDir world dir

{-|
  Run the game of Sokoban. Takes a single argument
  specifying the level data to read in.

  Usage: Sokoban <level.dat>
-}
main :: IO ()
main = do
  [f] <- getArgs
  level <- readLevel f
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle $ "Sokoban - " ++ f
  clearScreen
  let world = emptyWorld { wPlayer = (lStart level)
                         , wLevel = level
                         , wLevels = [level]
                         }
  gameLoop world
