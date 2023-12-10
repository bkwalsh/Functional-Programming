module MazeSolver where

import System.IO
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import System.Process
import System.Environment
import Data.List
import qualified Data.HashSet as HashSet

type Path = [Pos]
type Pos  = (Row, Col)
type Row  = Int
type Col  = Int


type Point = (Row, Col, Char)
type Board = HashSet.HashSet Point


-- given a string with board text from a file
-- convert the text to a Board 
fileTextToBoard :: String -> Board
fileTextToBoard = fileTextToBoard' 0 0 HashSet.empty
    where
      fileTextToBoard' r c set [] = set
      fileTextToBoard' r c set ('\n':xs) = fileTextToBoard' (r+1) 0 set xs
      fileTextToBoard' r c set (x:xs) = fileTextToBoard' r (c+1) (HashSet.insert (r,c,x) set) xs


-- given a Board return the position (x,y) of S
getStart :: Board -> Pos
getStart board =
  getStart' (HashSet.toList board)
  where
    getStart' [] = error "a polite error message"
    getStart' ((x,y,'S'):ls) = (x,y)
    getStart' (l:ls) = getStart' ls


-- globally defined variable for steps from a given position
steps = [(1, 0), (-1, 0), (0, 1), (0, -1)]


-- given a Board return a path if any, return nothing if no path possible
findPath :: Board -> Maybe Path
findPath board =
  let (start_x,start_y) = getStart board
      coords = [(start_x,start_y)]
      startmap = HashSet.insert (start_x,start_y,'S') HashSet.empty
      smove (dx, dy) = [(start_x + dx, start_y + dy)] in
  
  foldr (\lam acc -> dfs (smove lam) startmap coords <|> acc) Nothing steps
  
  where
    dfs [] visited path  = Nothing
    dfs ((x,y):xs) visited path
      | HashSet.member (x,y,'F') board = Just modpath
      | HashSet.member (x,y,' ') board && not (HashSet.member (x,y,' ') visited) =
        let imove (dx, dy) = [(x + dx, y + dy)] in
        foldr (\lam acc -> dfs (imove lam) (HashSet.insert (x,y,' ') visited) modpath <|> acc) Nothing steps

      | otherwise = dfs xs visited path
        where
          modpath = path ++ [(x,y)]
          

-- given a string representation of a board and a hashset representation
-- of a partial valid path, return a string that is a color version of the current
-- path state representation of the path            
colorBoard :: String -> HashSet.HashSet (Int, Int) -> String
colorBoard fileContents path =
  colorBoard' fileContents 0 0 [] 
    where
      colorBoard' [] r c build = build
      colorBoard' (x:xs) r c build
        | x=='\n' = colorBoard' xs (r+1) 0 (build ++ [x])
        | x=='X' = colorBoard' xs r (c+1) (build ++ "\x1b[31;40m" ++ [x] ++ "\x1b[0m")
        | HashSet.member (r,c) path = colorBoard' xs r (c+1) (build ++ "\x1b[42m" ++ [x] ++ "\x1b[0m")
        | x=='F' || x=='S' = colorBoard' xs r (c+1) (build ++ "\x1b[44m"  ++ [x] ++ "\x1b[0m")
        | otherwise = colorBoard' xs r (c+1) (build ++ "\x1b[47m" ++ [' '] ++ "\x1b[0m")


-- create an animation loop that given a valid path, a string representation of a board
-- and a Hashset representation of that path creates an animation by calling colorBoard
-- and moving steps from the path to the hashset until the path is exhausted
animationLoop :: Maybe Path -> String -> HashSet.HashSet (Int, Int) -> IO ()
animationLoop (Just[]) fileContents set =
  putStrLn "\x1b[42m YIPPIE!!!!\x1b[0m"

animationLoop Nothing fileContents set = do
  _ <- system "clear"
  putStrLn (colorBoard fileContents set)
  putStrLn "\x1b[31;40m No Path is Possible\x1b[0m"

animationLoop (Just ((a,b):xs)) fileContents set = do
  _ <- system "clear"
  let newset = HashSet.insert (a,b) set
  putStrLn (colorBoard fileContents newset)
  threadDelay 100000
  animationLoop (Just xs) fileContents newset

-- error msg helper function
errorMsg :: [Char]
errorMsg = 
  "\nexample usage: cabal run maze-solver -- mazes/maze-02.txt \n"
  ++ "or cabal run maze-solver -- mazes/maze-02.txt animate"

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:xs) -> do
      fileContents <- readFile filePath
      let maybePath = findPath (fileTextToBoard fileContents)
      case xs of
        ["animate"] -> animationLoop maybePath fileContents HashSet.empty
        [] -> print maybePath
        _ -> putStrLn errorMsg
    _ -> putStrLn errorMsg