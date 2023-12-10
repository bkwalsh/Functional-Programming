module Main where

import Data.List
import Interval
import System.IO

---- Input/Output Interface ----

-- Example: splitOn ',' "abc,def,ghi"  => ["abc", "def", "ghi"]

splitOn :: Char -> String -> [String]
splitOn splitChar [] = [[]]
splitOn splitChar (headChar : restChars)
  | splitChar == headChar = [[]] ++ splitOn splitChar restChars
  | otherwise = (headChar : currentWord) : restWords
  where
    currentWord : restWords = splitOn splitChar restChars

-- Vanilla Haskell doesn't allow instances on type synonyms,
-- so we can't make customized Show/Read instances.

readIS :: String -> IntervalSet Int
readIS = map read . splitOn ','

showIS :: IntervalSet Int -> String
showIS = concat . intersperse "," . map show . normalizeIS

-- Combine touching/overlapping regions and remove empty intervals.
-- Inverting twice effectively combines overlapping regions.

normalizeIS :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a
normalizeIS s
  | simplified == [] = [Range minBound minBound]
  | otherwise = simplified
  where
    inverse = difference allIS
    simplified = sort . removeEmptyIntervals . inverse . inverse $ s

processLine :: String -> String
processLine line =
  case words line of -- Hint: These can each be done in one line -- Another hint: Think map and ($) and (.)
    "intersection" : rest -> (.) showIS intersectionAll $ map readIS rest
    "union" : rest -> (.) showIS unionAll $ map readIS rest
    "difference" : rest -> (.) showIS differenceAll $ map readIS rest
    "disjoint" : rest -> (.) boolToString areAllDisjoint $ map readIS rest 
    "equal" : rest -> (.) boolToString areAllEqual $ map readIS rest 
    _ -> "Invalid input"

-- converts boolean data type to appropriate string for printing purposes
-- for disjoint and equal
boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

main :: IO ()
main = do
  line <- getLine
  case line of
    ('q' : _) -> return () -- "q" or "quit" to quit
    _ -> do
      putStrLn (processLine line)
      hFlush stdout -- "Flush" the output buffer (otherwise Windows may not show output)
      atEnd <- isEOF -- Check if at the end of a file before continuing (for testing)
      if atEnd
        then return ()
        else main -- Repeat
