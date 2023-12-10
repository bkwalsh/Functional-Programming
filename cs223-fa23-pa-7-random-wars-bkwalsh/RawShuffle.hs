module RawShuffle where

import System.Random

removeAt :: Int -> [a] -> (a, [a])
removeAt i lst =
  let (front,back) = splitAt i lst in
    (last front, init front ++ back)


removeRandom :: [a] -> StdGen -> ((a, [a]), StdGen)
removeRandom deck gen = do 
  let range = (1, length deck) :: (Int, Int) 
      (randomValue, newGen) = randomR range gen in
    (removeAt randomValue deck, newGen)


shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle = shuffle' []
  where
    shuffle' build [] gen = (build,gen)
    shuffle' build lst gen =
      let ((x,xs), newstate) = removeRandom lst gen in
        shuffle' (build ++ [x]) xs newstate