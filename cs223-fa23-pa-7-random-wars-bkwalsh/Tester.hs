module Tester where

import Cards
import Data.List
import MonadicShuffle
import RawShuffle
import State
import System.Random

--------------------------------------------------------------------------------

printSuccess :: Show a => String -> a -> Bool -> IO ()
printSuccess funcName input b =
  let
    strInput =
      show input
    strInput'
      | length strInput < 40 = strInput
      | otherwise            = take 40 strInput ++ "..."
  in
    putStrLn $ displayColor (if b then Green else Red) $
      intercalate " " [if b then "PASS" else "FAIL", funcName, strInput']

--------------------------------------------------------------------------------

data Color = Red | Green deriving (Eq, Ord)

displayColor :: Color -> String -> String
displayColor color str =
  let
    -- https://en.wikipedia.org/wiki/ANSI_escape_code
    -- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
    green  = "\ESC[32m"
    red    = "\ESC[31m"
    reset  = "\ESC[0m"
  in
    case color of
      Red   -> red ++ str ++ reset
      Green -> green ++ str ++ reset

--------------------------------------------------------------------------------

main :: IO ()
main = do

  printSuccess "fullDeckAcesHigh" () $
    show fullDeckAcesHigh ==
      "[2♣,2♦,2♥,2♠,3♣,3♦,3♥,3♠,4♣,4♦,4♥,4♠,5♣,5♦,5♥,5♠,6♣,6♦,6♥,6♠,7♣,7♦,7♥,7♠,8♣,8♦,8♥,8♠,9♣,9♦,9♥,9♠,10♣,10♦,10♥,10♠,J♣,J♦,J♥,J♠,Q♣,Q♦,Q♥,Q♠,K♣,K♦,K♥,K♠,A♣,A♦,A♥,A♠]"

  printSuccess "fullDeckAcesLow" () $
    show fullDeckAcesLow ==
      "[A♣,A♦,A♥,A♠,2♣,2♦,2♥,2♠,3♣,3♦,3♥,3♠,4♣,4♦,4♥,4♠,5♣,5♦,5♥,5♠,6♣,6♦,6♥,6♠,7♣,7♦,7♥,7♠,8♣,8♦,8♥,8♠,9♣,9♦,9♥,9♠,10♣,10♦,10♥,10♠,J♣,J♦,J♥,J♠,Q♣,Q♦,Q♥,Q♠,K♣,K♦,K♥,K♠]"

  printSuccess "RawShuffle.removeRandom [1..5] x 3" () $
    let g0 = mkStdGen 22300 in
    let (out1, g1) = RawShuffle.removeRandom [1..5] g0 in
    let (out2, g2) = RawShuffle.removeRandom [1..5] g1 in
    let (out3, g3) = RawShuffle.removeRandom [1..5] g2 in
    (out1, out2, out3) == ((4,[1,2,3,5]),(1,[2,3,4,5]),(3,[1,2,4,5]))

  printSuccess "RawShuffle.shuffle [1..5] x 3" () $
    let g0 = mkStdGen 22300 in
    let (list1, g1) = RawShuffle.shuffle [1..5] g0 in
    let (list2, g2) = RawShuffle.shuffle [1..5] g1 in
    let (list3, g3) = RawShuffle.shuffle [1..5] g2 in
    (list1, list2, list3) == ([4,3,1,5,2],[3,1,5,2,4],[1,5,4,2,3])

  printSuccess "MonadicShuffle.removeRandom [1..5] x 3" () $
    let
      action = do
        out1 <- MonadicShuffle.removeRandom [1..5]
        out2 <- MonadicShuffle.removeRandom [1..5]
        out3 <- MonadicShuffle.removeRandom [1..5]
        pure (out1, out2, out3)
    in
    evalState action (mkStdGen 22300) ==
      ((4,[1,2,3,5]),(1,[2,3,4,5]),(3,[1,2,4,5]))

  printSuccess "MonadicShuffle.shuffle [1..5] x 3" () $
    let
      action = do
        list1 <- MonadicShuffle.shuffle [1..5]
        list2 <- MonadicShuffle.shuffle [1..5]
        list3 <- MonadicShuffle.shuffle [1..5]
        pure (list1, list2, list3)
    in
    evalState action (mkStdGen 22300) ==
      ([4,3,1,5,2],[3,1,5,2,4],[1,5,4,2,3])

  printSuccess "shuffle fullDeckAcesHigh (mkStdGen 22300)" () $
    let g = mkStdGen 22300 in
    let deck = (evalState $ MonadicShuffle.shuffle fullDeckAcesHigh) g in
    show deck ==
      "[A♠,5♥,4♣,4♠,3♣,5♠,8♣,6♣,2♣,3♥,8♥,6♦,8♠,9♠,7♦,2♥,J♠,4♦,5♣,2♠,8♦,Q♥,K♠,6♠,7♣,3♦,K♦,Q♦,J♥,3♠,A♥,7♥,10♣,Q♣,6♥,2♦,K♥,4♥,9♥,Q♠,J♣,10♥,9♣,A♣,5♦,10♦,K♣,A♦,7♠,9♦,J♦,10♠]"

  printSuccess "shuffle fullDeckAcesHigh (mkStdGen 16100)" () $
    let g = mkStdGen 16100 in
    let deck = (evalState $ MonadicShuffle.shuffle fullDeckAcesHigh) g in
    show deck ==
      "[8♥,K♠,6♦,10♠,A♠,9♦,Q♥,7♠,8♦,6♠,10♦,3♠,8♠,Q♦,J♣,2♦,7♣,4♣,Q♣,8♣,10♣,9♥,3♥,K♦,9♣,5♣,K♣,7♦,6♥,2♣,3♣,5♦,4♥,K♥,3♦,Q♠,A♦,A♣,6♣,4♠,J♥,4♦,10♥,A♥,2♥,5♥,5♠,9♠,2♠,7♥,J♦,J♠]"

  printSuccess "shuffle fullDeckAcesLow (mkStdGen 22300)" () $
    let g = mkStdGen 22300 in
    let deck = (evalState $ MonadicShuffle.shuffle fullDeckAcesLow) g in
    show deck ==
      "[K♠,4♥,3♣,3♠,2♣,4♠,7♣,5♣,A♣,2♥,7♥,5♦,7♠,8♠,6♦,A♥,10♠,3♦,4♣,A♠,7♦,J♥,Q♠,5♠,6♣,2♦,Q♦,J♦,10♥,2♠,K♥,6♥,9♣,J♣,5♥,A♦,Q♥,3♥,8♥,J♠,10♣,9♥,8♣,K♣,4♦,9♦,Q♣,K♦,6♠,8♦,10♦,9♠]"

  printSuccess "shuffle fullDeckAcesLow (mkStdGen 16100)" () $
    let g = mkStdGen 16100 in
    let deck = (evalState $ MonadicShuffle.shuffle fullDeckAcesLow) g in
    show deck ==
      "[7♥,Q♠,5♦,9♠,K♠,8♦,J♥,6♠,7♦,5♠,9♦,2♠,7♠,J♦,10♣,A♦,6♣,3♣,J♣,7♣,9♣,8♥,2♥,Q♦,8♣,4♣,Q♣,6♦,5♥,A♣,2♣,4♦,3♥,Q♥,2♦,J♠,K♦,K♣,5♣,3♠,10♥,3♦,9♥,K♥,A♥,4♥,4♠,8♠,A♠,6♥,10♦,10♠]"
