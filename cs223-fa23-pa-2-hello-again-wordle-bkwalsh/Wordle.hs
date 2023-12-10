module Main where
import Data.Char (isUpper, toUpper,toLower)
import System.Environment
import System.Exit (die)
import System.IO
import System.Random
import Data.List (delete)
import Data.Char (isLetter)
import Prelude


maxGuesses = 6
wordLength = 5

debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()


-- My Helper Functions
--------------------------------------------------------------------------------

-- returns bool if string is in list of strings (querying for valid entry)
wordInList :: String -> [String] -> Bool
wordInList _ [] = False
wordInList str (x:xs)
  | str == x = True
  | otherwise = wordInList str xs

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
-- used for printing repeat letter case
noRepeatLetters :: String -> Bool
noRepeatLetters word = noRepeatLetters' word 0
  where
    noRepeatLetters' word i
      | i == length word = True
      | otherwise = ( noRepeatLetters' word (i + 1) && 
      noRepeatLettersHelper word (word !! i) (i + 1) )

-- Generates a random entry corresponding to a word in words.txt file
genValidEntry :: String -> IO String
genValidEntry txt = do
  indx <- randomRIO (0, 2314)
  return (lines txt !! indx)


-- Checks if manual specific test is a valid word index (within 0 to 2314)
-- Nothing to signal an invalid entered number (either not int or out of range)
checkValidParse :: String -> Maybe Int
checkValidParse s =
  case reads s :: [(Int, String)] of
    [(n, "")] -> if n < 0 || n > (2314)
        then Nothing
        else Just n
    _ -> Nothing


--------------------------------------------------------------------------------
-- Model and Actions
data Model = Model
  { wordList :: [String] -- wordlist dictionary
  , answer :: String -- generated answer string
  , board :: [String] -- state of board
  , firstPass :: Bool -- print initial message
  , wordBuilt :: Bool -- checks for entered word
  , printStringOutput :: String -- potential output if error message
  , currentguess :: String -- current characters in the line
  , keyBoardList :: [(Char, Color)] -- entered keys and their colors of keyboard
  } deriving (Show, Eq)

data Action =
  KeyPress Char

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do

  hSetBuffering stdin LineBuffering
  args <- getArgs
  fileContent <- readFile "words.txt"
  putStr "\ESC[30m"

  let modelInt = Model
        { wordList = lines fileContent
        , answer = ""
        , board = []
        , firstPass = True
        , wordBuilt = False
        , printStringOutput = ""
        , currentguess = ""
        , keyBoardList= []
        }

  case args of 
    [] -> do
      pickedWord <- genValidEntry fileContent
      controller (modelInt {answer = pickedWord}) >> return ()
    ["--how-to-play"] -> putStrLn printIntro
    [a]->
      case checkValidParse a of 
          Just v -> controller (modelInt {answer = (lines fileContent !! v)}) >> return ()
          Nothing -> die "Invalid game number"
    _-> die usageError      


--------------------------------------------------------------------------------
-- Controller

controller :: Model -> IO Model
controller modstate = do
    (if (not (null (board modstate))  && ((last (board modstate)) == (answer modstate)))
       || (length (board modstate) == 6) 
       then (do return modstate) 
       else (do

        view modstate
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        char <- getChar
        hSetEcho stdin True
        hSetBuffering stdin LineBuffering

        (if ((( char /= '\DEL')  && char /= '\n') &&  (length (currentguess modstate) == 5))
           || ((char == '\DEL') &&  ((currentguess modstate)==""))
           || (not (isLetter char) && (char /= '\n' && char /= '\DEL'))  
          then (do
          view modstate
          controller modstate) else 
          (do
          let newmodstate = update (KeyPress char) (modstate {firstPass = False})
          view newmodstate
          controller newmodstate))))


update :: Action -> Model -> Model
update action modstate =
  case action of
    KeyPress c
      | c == '\n' ->
          let localGuess = currentguess modstate
              generalmods= modstate
          in
          case () of
            _ | length localGuess < wordLength ->  
                  generalmods {printStringOutput = "Not enough letters\n"}
              | not (wordInList localGuess (wordList modstate)) ->
                  generalmods {printStringOutput = "Not in word list\n"}
              | otherwise ->
                  generalmods {board =  (board modstate) ++ [localGuess], printStringOutput = "",
                   keyBoardList= updateKeyBoard (answer modstate) localGuess (keyBoardList modstate) 0,
                   wordBuilt = True, currentguess= "" }
      | c == '\DEL' -> modstate {wordBuilt = False, currentguess= init (currentguess modstate),printStringOutput = "" }
      | otherwise -> modstate {wordBuilt = False, currentguess= currentguess modstate ++ [(toLower c)],printStringOutput = "" }


-- returns colors for letter for guessing function
checkLetter :: String -> (Int, Char) -> Color
checkLetter answer (i, letter) =
  let thisChar = (answer !! i)
   in if thisChar == letter
        then Green
        else
          if printLetterAnywhere answer 0 letter
            then Yellow
            else Gray

-- returns color if any in a given char,color list, none otherwise
queryCharForColor :: [(Char, Color)]-> Char -> Color
queryCharForColor [] char = None
queryCharForColor ((cc,col):xs) char
  | cc==char = col
  | otherwise = queryCharForColor xs char

-- removes a value (for updating color list)
removeValue :: Char -> [(Char, Color)] -> [(Char, Color)]
removeValue x = filter (\(a, _) -> a /= x)


-- updates a keyboard based on a given guess and prev state
updateKeyBoard :: String-> String-> [(Char, Color)]-> Int-> [(Char, Color)]
updateKeyBoard _ _ currstate 5= currstate
updateKeyBoard answer guess currstate i=
  let currCol=queryCharForColor currstate (guess !! i)
      inputCol = checkLetter answer (i, (guess !! i)) in
  if currCol== None then 
    updateKeyBoard answer guess (currstate ++ [((guess !! i),inputCol)]) (i+1)
  else if currCol>inputCol then 
    updateKeyBoard answer guess (currstate ) (i+1)
  else 
    updateKeyBoard answer guess ((removeValue (guess !! i) currstate) ++ [((guess !! i),inputCol)]) (i+1)


-- first pass on a double char case (checks for any exact matches and sets to !)
-- as a flag for green
firstPassDouble :: String -> String -> String -> Int -> (String,String)
firstPassDouble answer guess guesslist i
  | i==5 = (guesslist,answer)
  | (answer !! i) == (guess !! i) = 
    (firstPassDouble (replaceString i '!' answer) guess (guesslist ++ [(guess !! i)])  (i+1))

  | otherwise = firstPassDouble answer guess (guesslist ++ ['!'])  (i+1)


-- Helper function to see if letter is anywhere in the substring (repeat vers)
-- used in double char functionality 
printLetterAnywhereRepeat :: String -> Int -> Char -> Int
printLetterAnywhereRepeat answer i letter
    | i == length answer = -1
    | answer !! i == letter = i
    | otherwise = printLetterAnywhereRepeat answer (i + 1) letter


-- given the firstpass on double char case, checks for matches
-- and returns colors based on first pass and edge case behavior
secondPassDouble :: String -> String -> String-> String -> Int -> String
secondPassDouble answer guess firstPassList output i
  | i==5 = output
  | (guess !! i) == (firstPassList !! i) = 
  (secondPassDouble (replaceString i '!' answer) guess firstPassList (output ++ (colorLetter ((toUpper (guess !! i)),Green)))  (i+1))

  | ((printLetterAnywhereRepeat answer 0 (guess !! i)) /= -1) = 
    (secondPassDouble (replaceString (printLetterAnywhereRepeat answer 0 (guess !! i)) '!' answer) guess firstPassList (output ++ (colorLetter ((toUpper (guess !! i)),Yellow)) )  (i+1))

  | otherwise = 
    (secondPassDouble answer guess firstPassList  (output ++ (colorLetter ((toUpper (guess !! i)),Gray)) )  (i+1))


-- given a character and a index and a string, set the index of that string
-- to the character (for the ! flag)
replaceString :: Int -> Char -> String -> String
replaceString i ch str =
  let (start, end) = splitAt i str
  in start ++ [ch] ++ tail end


-- double case wrapper function
doubleWordCase :: String -> String -> IO ()
doubleWordCase answer guess =
  let (fP,newAnswer)= firstPassDouble answer guess "" 0 in
  putStrLn ("\ESC[48;5;15m"++ (secondPassDouble newAnswer guess fP "" 0)++"\x1b[40m"++"  "++"\x1b[47m"++"\ESC[48;5;15m")

--------------------------------------------------------------------------------
-- View

data Color = None | Gray | Yellow | Green
  deriving (Show,Eq, Ord)

-- colors a char and returns as a string
colorLetter :: (Char, Color) -> String
colorLetter (ca,None)    = "\ESC[48;5;15m"++"\ESC[30m"++" "++[ca]++" "++"\x1b[0m"
colorLetter (ca,Gray)    = "\x1b[48;5;235m"++"\ESC[97m"++" "++[ca]++" "++"\x1b[0m"
colorLetter (ca,Yellow)  = "\x1b[43m"++"\ESC[97m"++" " ++[ca]++" "++"\x1b[0m"
colorLetter (ca,Green)   = "\x1b[42m"++"\ESC[97m"++" " ++[ca]++" "++"\x1b[0m"

-- intro string for controller readiblity
printIntro :: String
printIntro =
  "\ESC[2J"
        ++ "\ESC[48;5;15m\ESC[2J" 
        ++ "\nHOW TO PLAY\n\n"
        ++ "Guess the WORDLE in 6 tries.\n\n"
        ++ "Each guess must be a valid 5 letter word. Hit the enter button to submit.\n\n"
        ++ "Examples\n\n "
        ++ colorLetter ('W',Green) ++ colorLetter ('E',Gray) ++ colorLetter ('A',Gray) ++ colorLetter ('R',Gray) ++ colorLetter ('Y',Gray) ++ "\ESC[48;5;15m" ++ "\ESC[30m"         
        ++ "  The letter W is in the word and in the correct spot.\n "
        ++ colorLetter ('P',Gray) ++ colorLetter ('I',Yellow) ++ colorLetter ('L',Gray) ++ colorLetter ('L',Gray) ++ colorLetter ('S',Gray) ++ "\ESC[48;5;15m" ++ "\ESC[30m"
        ++ "  The letter I is in the word but in the wrong spot.\n "
        ++ colorLetter ('V',Gray) ++ colorLetter ('A',Gray) ++ colorLetter ('G',Gray) ++ colorLetter ('U',Gray) ++ colorLetter ('E',Gray) ++ "\ESC[48;5;15m" ++ "\ESC[30m"
        ++ "  None of the letters are in the word in any spot.\n"


-- usage error string for controller readiblity
usageError :: String
usageError =
  "\ESC[2J"
        ++ "\ESC[48;5;15m\ESC[2J" 
        ++ "Usage:\n"
        ++ "\n"
        ++ "  ./wordle                  Play random game\n"
        ++ "  ./wordle gameNumber       Play specific game\n"
        ++ "  ./wordle --how-to-play    Display instructions\n"


-- Print Guess for a non double char case
printGuess :: String -> String -> IO ()
printGuess answer guess = printGuess' 0 ((zip [0..]) guess)
  where
    printGuess' i []
      | i == wordLength = putStr ("\x1b[40m" ++ "  "++ "\x1b[47m"++ "\ESC[48;5;15m"++ "\n")
    printGuess' i ((index, char) : xs)
      | i == index = do
          printLetter answer (i, char)
          printGuess' (i + 1) xs



-- Print Letter
printLetter :: String -> (Int, Char) -> IO ()
printLetter answer (i, letter) =
  let thisChar = (answer !! i)
   in if thisChar == letter
        then putStr (colorLetter ((toUpper letter),Green))
        else
          if printLetterAnywhere answer 0 letter
            then putStr (colorLetter ((toUpper letter),Yellow))
            else putStr (colorLetter ((toUpper letter),Gray))

-- Helper function to see if letter is anywhere in the substring 
printLetterAnywhere :: String -> Int -> Char -> Bool
printLetterAnywhere answer i letter =
  (i /= wordLength) && (let thisChar = (answer !! i)
                in ((thisChar == letter) ||
                printLetterAnywhere answer (i + 1) letter))


--------------------------------------------------------------------------------
-- View

view :: Model -> IO ()
view mod = do
  putStr "\ESC[48;5;15m\ESC[2J"
  if firstPass mod
    then do
      putStr "Guess the wordle!\n\n"
    else if (wordBuilt mod)
      then do
        if not (null (board mod))  && ((last (board mod)) == (answer mod))
          then do
            case length (board mod) of
              1 -> do
                    putStr "Genius!\n\n"
              2 -> do
                    putStr "Magnificent!\n\n"
              3 -> do
                    putStr "Impressive!\n\n"
              4 -> do
                    putStr "Splendid!\n\n"
              5 -> do
                    putStr "Great!\n\n"
              6 -> do
                    putStr "Phew!\n\n"
        else if length (board mod) == 6
          then do
            putStrLn ("Bummer, the answer was " ++ map toUpper (answer mod)++ "\n")
        else do
          putStr "Next guess?\n\n"
      else return ()

  if (printStringOutput mod) /= "" 
    then putStrLn (printStringOutput mod)
    else return()

  view' 0
  printKeyBoard (keyBoardList mod)
  where
    view' i
      | i == 7 = putStr ("\x1b[40m" ++  "                   " ++ 
      "\x1b[47m" ++ "\ESC[48;5;15m" ++ "\n" ++  "\ESC[48;5;15m" ++ 
      "                   " ++ "\ESC[48;5;15m" ++ "\n" )
      
      | i == 0 = do
        putStr ("\x1b[40m" ++  "                   " ++ "\x1b[47m" ++ "\ESC[48;5;15m" ++ "\n")
        view' (i + 1)
      | (i - 1) == length (board mod) = do
          putStr ("\x1b[40m" ++ "  "++ "\x1b[47m")
          putStr ( "\ESC[30m"++printCurrGuess (currentguess mod) ++ "\ESC[48;5;15m" ++ "\x1b[47m" ++ "\ESC[48;5;15m"++"\ESC[30m"++
            (createSpace (15- (3*length (currentguess mod))) "")++ "\ESC[48;5;15m")
          putStr ("\x1b[40m" ++ "  "++ "\x1b[47m"++ "\ESC[48;5;15m" ++ "\n" )
          view' (i + 1)

      | i > length (board mod) = do
          putStr ("\x1b[40m" ++ "  "++ "\x1b[47m" ++ "\ESC[48;5;15m")
          putStr ("\x1b[47m"++ "\ESC[48;5;15m" ++"               " ++ "\ESC[48;5;15m")
          putStr ("\x1b[40m" ++ "  "++ "\x1b[47m" ++ "\ESC[48;5;15m" ++  "\n")
          view' (i + 1)
      | otherwise = do
          putStr ("\x1b[40m" ++ "  "++ "\x1b[47m")
          if not (noRepeatLetters (board mod !! (i - 1))) then
            doubleWordCase (answer mod) (board mod !! (i - 1))
          else do
            printGuess (answer mod) ((board mod) !! (i - 1))
          putStr ("\ESC[48;5;15m" ++ "\x1b[40m")
          view' (i + 1)

-- creates space for printing colors of board based on the size of currently
-- inputed letters
createSpace:: Int-> String -> String
createSpace i str=
  if i==0 then str
  else createSpace (i-1) (str ++ " ")

-- prints the current guess of letters on board
printCurrGuess :: String -> String
printCurrGuess input=printCurrGuess' 0 ""
  where
    printCurrGuess' i build =
      if i == length input then build
      else printCurrGuess' (i +1) ("\ESC[30m"++ build ++ colorLetter ((toUpper (input !! i)),None))

-- prints the keyboard below board
printKeyBoard :: [(Char, Color)] -> IO ()
printKeyBoard lst= do
  let qwertyRows = ["qwertyuiop","asdfghjkl","zxcvbnm"]
  let partialDef= modRow lst
  let appliedRows= map partialDef qwertyRows
  mapIO_ putStrLn appliedRows

-- given a row (1 of 3 strings) in the keyboard, 
-- print out the row and assign the colors from the list
modRow :: [(Char, Color)]-> String-> String
modRow lst row = modRow' 0 ""
  where
    modRow' i build =
      if i==(length row) then (build++ "\x1b[47m" ++ "\ESC[30m" ++ "\ESC[48;5;15m")
      else modRow' (i +1) ("\ESC[30m"++build ++ (queryChar lst (row !! i))++ "\ESC[30m")

-- given a char and char list, assign that color to the char in the list and
-- return that string
queryChar :: [(Char, Color)]-> Char -> String
queryChar [] char = (colorLetter ((toUpper char),None))
queryChar ((cc,col):xs) char
  | cc==char = (colorLetter ((toUpper char),col))
  | otherwise = queryChar xs char


mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ = mapM_