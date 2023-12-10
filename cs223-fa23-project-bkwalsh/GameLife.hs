{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}
module GameLife where

import Brick
import Brick qualified as B
import Brick.AttrMap
  ( attrMap,
  )
import Brick.BChan
import Brick.Main
  ( App (..),
    customMainWithDefaultVty,
    halt,
    showFirstCursor,
  )
import Brick.Types
  ( BrickEvent (..),
    EventM,
    Widget,
  )
import Brick.Widgets.Border
import Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( emptyWidget,
    hBox,
    padLeftRight,
    padTopBottom,
    withBorderStyle,
    (<+>),
    (<=>),
  )
import Brick.Widgets.ProgressBar qualified as P
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM, void, when)
import Data.Char
import Data.HashMap.Strict qualified as H
import Data.List (findIndex, foldl')
import Graphics.Vty qualified as V hiding (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import PreSetSeeds
import PresetParser hiding (CordHashMap)
import State
import System.Console.ANSI
import System.Environment
import System.Exit
import System.IO
import System.Random
import Text.Read (readMaybe)

-- TYPES
-- hashmap containing alive values for (x,y) as string "x"
type CordHashMap = H.HashMap (Int, Int) String

type InteriorST = (CordHashMap, StdGen, (Int, Int), Int, [(Int, Int)])

type Rand a = State StdGen a

-- AppState, handles state of current game
data AppState = AppState
  { _st ::
      ( CordHashMap, -- hashmap of alive values
        StdGen, -- gen for random seed generation
        (Int, Int), -- (x,y) bounds for board
        Int, -- density for generating values (not in percent)
        [(Int, Int)] -- list of dead and alive cells at each round for end game stats
      )
  }

-- BRICK LIBRARY FUNCTIONALITY
makeLenses ''AppState

-- draws board through brick functions based on hashmap values
drawUI :: AppState -> [Widget ()]
drawUI state =
  [ C.center $
      border $
        vBox $
          [ hBox
              [ case H.lookup (row, col) m of
                  Just value -> B.str value
                  Nothing -> B.str "_"
                | col <- [0 .. a]
              ]
            | row <- [0 .. b]
          ]
  ]
  where
    (m, _, (a, b), _, _) = _st state

-- calls function based on key presses and then updates board
appEvent :: BrickEvent () () -> EventM () AppState ()
appEvent e =
  case e of
    -- quit
    (VtyEvent (V.EvKey (V.KChar 'q') [])) -> do
      st %= epochStep
      halt
    -- regenerate (add x density to random cells)
    (VtyEvent (V.EvKey (V.KChar 'r') [])) -> do st %= genRandomSquares
    -- kill all cells
    (VtyEvent (V.EvKey (V.KChar 'k') [])) -> do st %= setEmptyGrid
    -- populate all cells
    (VtyEvent (V.EvKey (V.KChar 'f') [])) -> do st %= fillGrid
    -- step to next state in conway game
    (VtyEvent (V.EvKey (V.KChar 's') [])) -> do st %= epochStep

    otherwise -> return ()


-- appState initiation and updating through brick functionality
app :: App AppState () ()
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr []
    }

-- BOARD MANAGEMENT FUNCTIONS

-- kills all values in an InteriorST
setEmptyGrid :: InteriorST -> InteriorST
setEmptyGrid (cm, st, (x, y), per, lst) = (H.empty, st, (x, y), per, lst)

-- populate all values in an InteriorST
fillGrid :: InteriorST -> InteriorST
fillGrid (cm, st, (x, y), per, lst) =
  let fullgrid = H.fromList [((i, j), "X") | i <- [0 .. y], j <- [0 .. x]]
   in (fullgrid, st, (x, y), per, lst)

-- generates a random amount of new alive cells in a board
-- based on the distribution in InteriorST
genRandomSquares :: InteriorST -> InteriorST
genRandomSquares (map, generatorVal, (x, y), percent, lst) = do
  let newMap = State.evalState (populate map iterations) generatorVal
   in (newMap, (snd . split) generatorVal, (x, y), percent, lst)
  where
    iterations = floor (fromIntegral (x * y) * (fromIntegral percent / 100))
    populate :: CordHashMap -> Int -> Rand CordHashMap
    populate m 0 = return m
    populate m i =
      if (H.size m + i) >= (x * y)
        then return (H.fromList [((i, j), "X") | i <- [0 .. y], j <- [0 .. x]])
        else do
          (x', y') <- removeRandom (x, y)
          case H.lookup (x', y') m of
            Just value -> populate m i
            Nothing -> populate (H.insert (x', y') "X" m) (i - 1)

-- given limits of board (x,y), generate a random position
-- within that board (b,a)
removeRandom :: (Int, Int) -> Rand (Int, Int)
removeRandom (x, y) = do
  a <- State (randomR (0, x))
  b <- State (randomR (0, y))
  return (b, a)

-- boolean helper function to check if a cord is alive
isAliveHelper :: CordHashMap -> (Int, Int) -> Bool
isAliveHelper cm v = do
  case H.lookup v cm of
    Just _ -> True
    Nothing -> False

-- calculates neighbors (between 0 to 8) around a given cord for board
getNeighbors :: (Int, Int) -> CordHashMap -> Int
getNeighbors (x, y) cm =
  length $ filter (isAliveHelper cm) [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

-- adds a val to a CordHashMap or not depending on bool flag switching cell
manageCell :: CordHashMap -> ((Int, Int), Bool) -> CordHashMap
manageCell hashMap (coords, cellState) =
  if cellState then H.insert coords "X" hashMap else hashMap

-- given a CordHashMap and the dimensions of the board
-- return a tuple of (alive,dead) cells for stats
getAlivecount :: CordHashMap -> (Int, Int) -> (Int, Int)
getAlivecount cm (x, y) =
  let s = H.size cm
   in (s, (x * y) - s)

-- Kills a Cell or not depending on the amount of its neighbors
killOrNot :: CordHashMap -> ((Int, Int), Bool) -> ((Int, Int), Bool)
killOrNot cm ((x, y), cellLife) =
  let neighbors = getNeighbors (x, y) cm
   in ( (x, y),
        ((neighbors >= 2) || (neighbors < 4))
          && ((neighbors == 2 && cellLife) || (neighbors == 3))
      )

-- "steps" game forward, by calculating which cells should die based on game rules
-- also updates lst of dead/ alive cells for end game stats
epochStep :: InteriorST -> InteriorST
epochStep (cmInitial, st, (a, b), dens, stattracking) =
  let liveCells = [(elem, isAliveHelper cmInitial elem) | elem <- ([(x, y) | x <- [1 .. a], y <- [1 .. b]])]
      boolMap = map (killOrNot cmInitial) liveCells
      finalState = foldl' manageCell H.empty boolMap
      roundStats = getAlivecount cmInitial (a, b)
   in (finalState, st, (a, b), dens, stattracking ++ [roundStats])

-- IO MANAGEMENT FUNCTIONS

-- string for opening sequence of game
gameOfLifeDescription :: String
gameOfLifeDescription =
  "Conway's Game of Life is a classic cellular automaton devised by\n"
    ++ "mathematician John Conway in 1970. It's initially a zero-player game, meaning its\n"
    ++ "evolution is determined solely by its initial state, with no further\n"
    ++ "input from players. However, this version gives the player option\n "
    ++ "to randomly populate the board between turns. \n"
    ++ "The game unfolds on a two-dimensional grid of cells,\n"
    ++ "each of which can be either alive or dead.\n\n"
    ++ "The game operates based on simple rules. A living cell survives to the next\n"
    ++ "generation if it has two or three neighbors; otherwise, it dies due to\n"
    ++ "underpopulation or overcrowding. A dead cell becomes alive in the next\n"
    ++ "generation if it has exactly three living neighbors, simulating reproduction.\n\n\n"
    ++ "Here are some example commands:\n"
    ++ "[q] -> quit terminal and exit the game\n"
    ++ "[r] -> regenerate x percent of cells on the board\n"
    ++ "[s] -> step forward once in the game, following Conway rules\n"
    ++ "[f] -> populates all cells in the board \n"
    ++ "[k] -> kill all cells in the board \n\n\n"

-- given a list of (alive,dead) cells for each round of the game
-- generate a string with various statistics for the end of the game er
generateEndStates :: [(Int, Int)] -> String
generateEndStates lst =
  let overallDead = show $ foldl (\acc (_, x) -> acc + x) 0 lst
      overallAliveInt = foldl (\acc (x, _) -> acc + x) 0 lst
      overallAlive = show overallAliveInt
      extinction = maybe "All Rounds were Populated" show $ findIndex (\(x, _) -> x == 0) lst
      averageCells = show (realToFrac overallAliveInt / fromIntegral (length lst))
      epochsPlayed = show $ length lst
      maxcells = show $ maximum (map fst lst)
      mincells = show $ minimum (map fst lst)
   in "Thank you for playing the simulation! Here are some stats about your time "
        ++ "\nduring the simulation: \n \n"
        ++ "Overall UnPopulated Cells: "
        ++ overallDead
        ++ "\nOverall Populated Cells: "
        ++ overallAlive
        ++ "\nExtinction Reached at: "
        ++ extinction
        ++ "\nAverage Cells: "
        ++ averageCells
        ++ "\nEpochs Played: "
        ++ epochsPlayed
        ++ "\nMax Cells on the Board: "
        ++ maxcells
        ++ "\nMin Cells on the Board: "
        ++ mincells

-- reads y/n or q from user to handle custom or default configurations
readYN :: IO Char
readYN = do
  hSetBuffering stdin LineBuffering
  input <- getLine
  case input of
    "q" -> putStrLn "User decided to quit." >> exitSuccess
    "y" -> return 'y'
    "n" -> return 'n'
    _ -> do
      putStrLn "Invalid input. Please enter 'y', 'n', or 'q' to quit."
      readYN

-- loop to read an int from user (for custom board dimensions) or quit
readInt :: IO Int
readInt = do
  hSetBuffering stdin LineBuffering
  input <- getLine
  case input of
    "q" -> putStrLn "User decided to quit." >> exitSuccess
    _ -> case readMaybe input of
      Just n -> return n
      Nothing -> do
        putStrLn "Invalid input. Please enter a valid integer or 'q' to quit."
        readInt

-- reads commands for a preset Seed
readPreset :: IO Char
readPreset = do
  hSetBuffering stdin LineBuffering
  input <- getLine
  case input of
    "q" -> putStrLn "User decided to quit." >> exitSuccess
    "g" -> return 'g'
    "b" -> return 'b'
    "h" -> return 'h'
    "p" -> return 'p'
    "y" -> return 'y'
    _ -> do
      putStrLn "Invalid input. Please enter a valid seed or 'q' to quit."
      readYN

-- IO for handling getting configurations (default or prompted by user)
getConfigurations :: IO (Int, Int, Int)
getConfigurations = do
  putStrLn "Would you like to play with baseline configurations? ([y]/[n]) "
  b <- readYN
  case b of
    'y' -> return (25, 25, 40)
    'n' -> do
      putStrLn "Enter a Width of the grid:"
      x <- readInt
      putStrLn "Enter a Length of the grid:"
      y <- readInt
      putStrLn "Enter a Density for populating at each turn:"
      dens <- readInt
      return (x, y, dens)

-- String formatting for preset options
seedString :: String
seedString =
  "Select one of the following seeds:\n"
    ++ "[g] -> glider\n"
    ++ "[b] -> blinker\n"
    ++ "[p] -> pulser\n"
    ++ "[y] -> glidergun \n"
    ++ "[h] -> hammerhead \n\n"

-- IO loop for user handling custom seeds loading or random generation
getConfigs :: StdGen -> IO InteriorST
getConfigs gen = do
  putStrLn "Would you like to load a preset seed? ([y]/[n]) "
  b <- readYN
  case b of
    'y' -> do
      putStrLn seedString
      s <- readPreset
      case s of
        'g' -> do
          let (cords, seed) = parserHandler glider
          return (seed, gen, cords, 25, [])
        'b' -> do
          let (cords, seed) = parserHandler blinker
          return (seed, gen, cords, 25, [])
        'p' -> do
          let (cords, seed) = parserHandler pulser
          return (seed, gen, cords, 25, [])
        'y' -> do
          let (cords, seed) = parserHandler gliderGun
          return (seed, gen, cords, 25, [])
        'h' -> do
          let (cords, seed) = parserHandler hammerHead
          return (seed, gen, cords, 25, [])
    'n' -> do
      (x, y, dens) <- getConfigurations
      return (genRandomSquares (H.empty, gen, (x, y), dens, []))

-- MAIN

main :: IO ()
main = do
  putStr "\ESC[2J\ESC[H"
  putStr gameOfLifeDescription
  hSetBuffering stdin NoBuffering
  gen <- getStdGen
  globalHashMap <- getConfigs gen
  (state, vis) <- customMainWithDefaultVty Nothing app (AppState {_st = globalHashMap})
  let (_, _, _, _, generations) = _st state
  putStr "\ESC[2J\ESC[H"
  putStrLn (generateEndStates generations)