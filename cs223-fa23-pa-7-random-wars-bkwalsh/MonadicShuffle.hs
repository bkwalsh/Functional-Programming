module MonadicShuffle where

import State
import System.Random

type Rand a = State StdGen a

removeAt :: Int -> [a] -> (a, [a])
removeAt i lst =
  let (front,back) = splitAt i lst in
    (last front, init front ++ back)


-- helper function for generating random range
randomRange :: Random a => (a,a) ->  Rand a
randomRange range = State (randomR range)


removeRandom :: [a] -> Rand (a, [a])
removeRandom deck = do
  a <- randomRange (1, length deck)
  pure (removeAt a deck)


shuffle :: [a] -> Rand [a]
shuffle = shuffle' []
  where
    shuffle' build [] = return build
    shuffle' build lst = fmap (build ++) (removeRandom lst >>= \(x, xs) -> shuffle' [x] xs)