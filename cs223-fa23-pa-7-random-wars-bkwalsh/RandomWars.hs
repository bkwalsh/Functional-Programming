{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module RandomWars where

import Cards 
import MonadicShuffle
import State
import System.Random

-- using WarMode as 3rd singleton data instance for war simulation card ordering
data WarMode = WarMode

instance Ord (Card WarMode) where
  _ <= (Card (PipCard 1) _) = True  
  (Card (PipCard 1) _) <= _ = False  
  (Card a _) <= (Card b _) = a <= b


howLongDoWarsLast :: Int
howLongDoWarsLast = 376

-- README

-- Aces are high, as per standard game of war
-- After a person runs out of cards in their primary deck, they shuffle 
-- their winnings deck which then becomes their primary deck (note that players
-- shuffle individually, and only when a player runs through their primary deck
-- does that player shuffles + adds their winnings)
-- This shuffling means games will always converge in practice
-- A step is counted as a card being played - note that steps are NOT counted 
-- for 1) shuffling or 2) putting cards down for war
-- however, engaging a war and then winning the war will count as 2 steps
-- because 1 card has been played to tie and then 1 card to win

-- Another design is that if a war would place someone at zero or negative cards
-- (say entering a war with only 2 cards left), the war will be cut short so as to leave 
-- each player with at least 1 card. Note this cutoff is equal for both
-- players, so if a war starts and the deck sizes are 3 and 47, then each player
-- would put 2 cards and determine the winner of the war on the following card.

-- note that I am also not using killall with a threshhold because with shuffling 
-- a persons winnings deck all games converge to ending in practice 

-- howLongDoWarsLast is an average across simulating 40000 games


main :: IO ()
main = do
  stdGen <- initStdGen
  let anumberoftrials = 40000 
  let ns = evalState (simulateGames anumberoftrials []) stdGen
  putStrLn $ "Average: " ++ show (average ns)


-- average helper function for list of game turns
average :: Integral a => [a] -> Double
average [] = 0
average xs = realToFrac (sum xs) / fromIntegral (length xs)


-- given a number n and an empty list, recursively generate a list of n 
-- simulated games stored by turn
simulateGames :: Int -> [Int] -> Rand [Int]
simulateGames n lst =
  simulateGame >>= \out ->
  if n > 0
    then simulateGames (n - 1) (lst ++ [out])
    else return lst


-- simulate a game of war with shuffling a deck, splitting a deck in half
-- and kicking off the playGame loop
simulateGame :: Rand Int
simulateGame =
  (\deck ->
  shuffle deck >>= \shuffleDeck ->
  let (firstHalf, secondHalf) = splitAt (length shuffleDeck `div` 2) shuffleDeck
  in playGame firstHalf [] secondHalf [] [] 0) fullDeckAcesHigh


-- given lists of cards in a war, remove up to three or the equal amount of cards
-- to simulate cards down in a war
warCards:: [Card a] -> [Card a] -> [Card a] -> ([Card a],[Card a],[Card a])
warCards lst [] build = (build,lst,[])
warCards [] lst build = (build,[],lst)
warCards lst [x] build = (build,lst,[x])
warCards [x] lst build =  (build,[x],lst)
warCards (ax:as) (bx:bs) build =
  if length build == 4 then ((build ++ [ax]) ++ [bx],as,bs)
  else warCards as bs ((build ++ [ax]) ++ [bx])


-- primary driver for a game of war, given 2 decks with a winning pile and 
-- a primary pile as well as a current war stack, and the number of current 
-- steps in the game, simulate war games and manipulating decks until there is 
-- a winner and then returning the number of steps
playGame :: [Card a] -> [Card a] -> [Card a] -> [Card a] -> [Card a] -> Int -> Rand Int
playGame [] [] _ _ warStack steps = return steps
playGame _ _ [] [] warStack steps = return steps

playGame [] astack bdeck bstack warStack steps = do
  shuffleDeck <- shuffle astack
  playGame shuffleDeck [] bdeck bstack warStack steps

playGame adeck astack [] bstack warStack steps = do
  shuffleDeck <- shuffle bstack
  playGame adeck astack shuffleDeck [] warStack steps

playGame ((Card acard suita):as) astack ((Card bcard suitb):bs) bstack warStack steps = do
  if Card @WarMode acard suita == Card @WarMode bcard suitb
    then
      let (war,adeck,bdeck) = warCards as bs [] in
      playGame adeck astack bdeck bstack ((([Card acard suita] ++ [Card bcard suitb]) ++ warStack) ++ war) (steps+1)
      else if Card @WarMode acard suita <= Card @WarMode bcard suitb
        then playGame as astack bs (((bstack ++ warStack ) ++ [Card acard suita]) ++ [Card bcard suitb]) [] (steps+1)
          else playGame as (((astack ++ warStack) ++ [Card acard suita]) ++ [Card bcard suitb]) bs bstack [] (steps+1)