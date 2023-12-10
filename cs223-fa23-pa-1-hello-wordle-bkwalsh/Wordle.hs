-- Source: I used https://hackage.haskell.org/package/base to find helpful
-- supplimentary libraries (Data.Char for example)

module Main where

import Data.Char (isUpper, toUpper)
import System.Environment
import System.Exit (die)
import System.IO
import System.Random
import Prelude

maxGuesses = 6

wordLength = 5

wordListLength = 2315

debugLog :: String -> IO ()
debugLog s =
  let debug = False
   in if debug then putStrLn s else return ()

-- My Helper Functions
--------------------------------------------------------------------------------

-- Helper functon for noRepeatLetters
-- checks a single letter in a string against all following letters
-- to see if there is a repeat letter
-- if so return false
noRepeatLettersHelper :: String -> Char -> Int -> Bool
noRepeatLettersHelper word c i
  | i == length word = True
  | (word !! i) == c = False
  | otherwise = noRepeatLettersHelper word c (i + 1)

-- Given a string return True if no repeat letters in string (for word validity)
-- runs repeatletterhelper on each char
noRepeatLetters :: String -> Bool
noRepeatLetters word = noRepeatLetters' word 0
  where
    noRepeatLetters' word i
      | i == length word = True
      | otherwise = noRepeatLetters' word (i + 1) && noRepeatLettersHelper word (word !! i) (i + 1)

-- Generates a valid entry corresponding to a word in words.txt file
-- considers repeat letters potentially and will rerun until random word is valid
-- and then return that word
genValidEntry :: String -> IO String
genValidEntry txt = do
  indx <- randomRIO (0, wordListLength - 1)
  let word = ((lines txt) !! indx)
  if noRepeatLetters word
    then return word
    else genValidEntry txt

-- returns bool if string is in list of strings (querying for valid entry)
wordInList :: String -> [String] -> Bool
wordInList _ [] = False
wordInList str (x : xs)
  | str == x = True
  | otherwise = wordInList str xs

-- Checks if inputted test is a valid word index (within 0 to 2314)
-- Note that the (wordListLength - 1) is for 0 indexing in the list.
-- -1 is a flag to signal an invalid entered number (either not int or out of range)
checkValidParse :: String -> Int
checkValidParse s =
  case reads s :: [(Int, String)] of
    [(n, "")] ->
      if n < 0 || n > (wordListLength - 1)
        then -1
        else n
    _ -> -1

--------------------------------------------------------------------------------
-- Main
main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  args <- getArgs
  fileContent <- readFile "words.txt"
  if null args
    then do
      pickedWord <- genValidEntry fileContent
      controller (lines fileContent) pickedWord []
    else
      if length args > 1
        then die usageError
        else
          if head args == "--how-to-play"
            then putStrLn printIntro
            else
              if checkValidParse (head args) == -1
                then die "Invalid game number"
                else do
                  let indexWrd = ((lines fileContent) !! (checkValidParse (head args)))
                  if noRepeatLetters indexWrd
                    then do
                      controller (lines fileContent) indexWrd []
                    else do
                      die (map toUpper indexWrd ++ " has repeat letters")

--------------------------------------------------------------------------------
-- Controller
controller :: [String] -> String -> [String] -> IO ()
controller wordList answer board = controller' 0 []
  where
    controller' i board = do
      printBoard answer board
      if not (null board) && ((board !! (length board - 1)) == answer)
        then do
          case length board of
            1 -> putStr "Genius!\n"
            2 -> putStr "Magnificent!\n"
            3 -> putStr "Impressive!\n"
            4 -> putStr "Splendid!\n"
            5 -> putStr "Great!\n"
            6 -> putStr "Phew!\n"
        else
          if length board == 6
            then putStrLn ("Bummer, the answer was " ++ map toUpper answer)
            else do
              if i == 0
                then printPrompt answer
                else putStr "Next guess?\n"
              putStr "> "
              guess <- getLine
              let (newAnswer, newBoard) = maybeUpdateBoard wordList answer board guess
              do
                if newAnswer /= ""
                  then putStr newAnswer
                  else return ()
                controller' (i + 1) newBoard

maybeUpdateBoard ::
  [String] -> String -> [String] -> String -> (String, [String])
maybeUpdateBoard wordList answer board guess =
  if length guess < wordLength
    then do
      ("Not enough letters\n", board)
    else
      if length guess > wordLength
        then do
          ("Too many letters\n", board)
        else
          if any isUpper guess
            then do
              ("Not all lowercase letters\n", board)
            else
              if not (noRepeatLetters guess)
                then do
                  ("Repeat letters not allowed\n", board)
                else
                  if not (wordInList guess wordList)
                    then do
                      ("Not in word list\n", board)
                    else do
                      ("", board ++ [guess])

--------------------------------------------------------------------------------
-- View

-- intro string for controller readiblity
printIntro :: String
printIntro =
  "\nHOW TO PLAY\n\n"
    ++ "Guess the WORDLE in 6 tries.\n\n"
    ++ "Each guess must be a valid 5 letter word. Hit the enter button to submit.\n\n"
    ++ "Examples\n\n"
    ++ "  W [e][a][r][y]  The letter W is in the word and in the correct spot.\n"
    ++ " [p] i [l][l][s]  The letter I is in the word but in the wrong spot.\n"
    ++ " [v][a][g][u][e]  None of the letters are in the word in any spot."

-- usage error string for controller readiblity
usageError :: String
usageError =
  "Usage:\n"
    ++ "\n"
    ++ "  ./wordle                  Play random game\n"
    ++ "  ./wordle gameNumber       Play specific game\n"
    ++ "  ./wordle --how-to-play    Display instructions"

-- Iniital print prompt string
printPrompt :: String -> IO ()
printPrompt str =
  putStr "Guess the wordle!\n"

-- Print board
printBoard :: String -> [String] -> IO ()
printBoard answer board =
  printBoard' 0
  where
    printBoard' i
      | i == 7 = putStr "###################\n\n"
      | i == 0 = do
          putStr "\n###################\n"
          printBoard' (i + 1)
      | (i - 1) >= length board = do
          putStr "##               ##\n"
          printBoard' (i + 1)
      | otherwise = do
          putStr "##"
          printGuess answer (board !! (i - 1))
          printBoard' (i + 1)

-- Print Guess
printGuess :: String -> String -> IO ()
printGuess answer guess = printGuess' 0
  where
    printGuess' i
      | i == wordLength = putStr "##\n"
      | otherwise = do
          printLetter answer (i, guess !! i)
          printGuess' (i + 1)

-- Print Letter
printLetter :: String -> (Int, Char) -> IO ()
printLetter answer (i, letter) =
  let thisChar = (answer !! i)
   in if thisChar == letter
        then putStr (" " ++ [toUpper letter] ++ " ")
        else
          if printLetterAnywhere answer 0 letter
            then putStr (" " ++ [letter] ++ " ")
            else putStr ("[" ++ [letter] ++ "]")

-- Helper function to see if letter is anywhere in the substring
printLetterAnywhere :: String -> Int -> Char -> Bool
printLetterAnywhere answer i letter =
  (i /= wordLength)
    && ( let thisChar = (answer !! i)
          in ( (thisChar == letter)
                 || printLetterAnywhere answer (i + 1) letter
             )
       )