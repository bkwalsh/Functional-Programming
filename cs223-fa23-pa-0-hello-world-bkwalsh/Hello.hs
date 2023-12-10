module Main where

data Level
  = Undergraduate
  | Masters
  | PhD
  | Other
      deriving Show

name :: String
name = "Bayard Walsh"

level :: Level
level = Undergraduate

major :: String
major = "Computer Science BA, wuth a Minor in Architectural Studies"

why :: String
why = "I've loved every CS course I've taken so far and liked the problem solving/math aspect of coding. I enjoyed programming languages so taking functional programming felt right."

distance :: Int -> Int -> Int
distance rate time = rate * time

main :: IO ()
main = do
  putStrLn "Hello, world!"
  putStrLn ""
  putStrLn ("My name is " ++ name ++ ".")
  putStrLn ("I am a " ++ show level ++ " student.")
  putStrLn ("I'm studying " ++ major ++ ".")
  putStrLn ("I'm in this class because " ++ why)
  putStrLn ""
  putStrLn $
    "If you travel 15mph for 30 hours you will go " ++
    show (distance 15 30) ++ " miles."
