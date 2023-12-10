module Interval where

---- Intervals ----

data Interval a
  = Range a a

isEmptyInterval :: Ord a => Interval a -> Bool
isEmptyInterval (Range start end) =
  start >= end

instance Ord a => Eq (Interval a) where
  i1@(Range start1 end1) == i2@(Range start2 end2) =
    (isEmptyInterval i1 && isEmptyInterval i2)
      || (start1 == start2 && end1 == end2)

-- Intervals are ordered by start. This is necessary for normalizeIS.
--
instance Ord a => Ord (Interval a) where
  Range start1 end1 <= Range start2 end2 =
    (start1, end1) <= (start2, end2)

instance (Show a, Ord a, Bounded a) => Show (Interval a) where
  show i@(Range start end)
    | isEmptyInterval i = "Empty"
    | start == minBound && end == maxBound = "All"
    | start == minBound = "<" ++ show end
    | end == maxBound = ">=" ++ show start
    | otherwise = show start ++ "<=_<" ++ show end

instance (Read a, Ord a, Bounded a) => Read (Interval a) where
  -- Read is a little less straightforward because it uses parsers,
  -- which we'll learn about later. Just replace the `undefined`s below
  -- with what you want to return and everything will work.
  -- Hint: Use `read` to get the value of an endpoint from `next`.
  readsPrec _ "Empty" = [(Range minBound minBound, "")]
  readsPrec _ "All" = [(Range minBound maxBound, "")]
  readsPrec _ ('>' : '=' : next) = [(Range (read next) maxBound, "")]
  readsPrec _ ('<' : next) = [(Range minBound (read next), "")]
  readsPrec _ str =
    -- Don't worry about this case. It is a bit clunky.
    -- We'll learn a better approach later in the course.
    case reads str of
      (start, '<' : '=' : '_' : '<' : rest) : _ ->
        case reads rest of
          [] -> error "error parsing interval"
          (end, _) : _ -> [(Range start end, "")]
      _ ->
        error "error parsing interval"

-- note apply range b a as empty interval trick if not 
intersectIntervals :: Ord a => Interval a -> Interval a -> Interval a
intersectIntervals (Range a b) (Range c d) =
  if b < c || d < a
    then Range b a
    else Range (max a c) (min b d)

---- Interval Sets ----

-- An interval set might have intervals that overlap or touch. Don't worry
-- about simplifying these cases in the following functions. That is handled
-- just before displaying by normalizeIS.

type IntervalSet a =
  [Interval a]

toIS :: Interval a -> IntervalSet a
toIS =
  (: [])

emptyIS :: IntervalSet a
emptyIS =
  []

allIS :: Bounded a => IntervalSet a
allIS =
  [Range minBound maxBound]

removeEmptyIntervals :: Ord a => IntervalSet a -> IntervalSet a
removeEmptyIntervals =
  filter $ not . isEmptyInterval

intersectISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a
intersectISI [] i = []
intersectISI is i = intersectISI' is i []
  where
    intersectISI' [] _ build = build
    intersectISI' ( (Range c d) : xs) (Range a b) build = case () of
      _
        | c <= a && a <= d && d <= b ->
          intersectISI' xs (Range a b) (build ++ [Range a d])
        | a <= c && d <= b ->
          intersectISI' xs (Range a b) (build ++ [Range c d])
        | c <= a && b <= d ->
          intersectISI' xs (Range a b) (build ++ [Range a b])
        | a <= c && c <= b && b <= d ->
          intersectISI' xs (Range a b) (build ++ [Range c b])
        | otherwise -> intersectISI' xs (Range a b) build


-- The complement of an interval must return an interval set because it may
-- result in two disjoint intervals.
complementInterval :: (Bounded a, Ord a) => Interval a -> IntervalSet a
complementInterval (Range start end)
  | start == minBound = [Range end maxBound]
  | end == maxBound = [Range minBound start]
  | otherwise = [Range minBound start, Range end maxBound]


-- An interval minus an interval must return an interval set because the second
-- could cut a hole in the middle of the first.
--
-- Big Hint: Use complements and intersections.
--
-- IMPORTANT NOTE: There cannot be any empty intervals left over in the ouptut
-- of this function. Leaving them does not affect the results, but it may make
-- your program too slow! You are welcome to use removeEmptyIntervals for this.
--
differenceIntervals ::
  (Ord a, Bounded a) =>
  Interval a ->
  Interval a ->
  IntervalSet a
differenceIntervals a b =
  removeEmptyIntervals (intersectISI (complementInterval b) a)


-- Interval set minus an interval.
--
differenceISI ::
  (Ord a, Bounded a) =>
  IntervalSet a ->
  Interval a ->
  IntervalSet a
differenceISI iset icase =
  concatMap (`differenceIntervals` icase) iset


---- Helpers for interval sets ----
intersection ::
  Ord a =>
  IntervalSet a ->
  IntervalSet a ->
  IntervalSet a
intersection s1 [] = []
intersection s1 (x : xs) = intersection' s1 x (removeEmptyIntervals xs) []
  where
    intersection' s_op l [] build =
      if isEmptyInterval l then [l] else intersectISI s_op l ++ build
    intersection' s_op l (ll : ls) build =
      intersection' s_op ll ls (intersectISI s_op l ++ build)


union :: IntervalSet a -> IntervalSet a -> IntervalSet a
union a b = a ++ b


difference ::
  (Ord a, Bounded a) =>
  IntervalSet a ->
  IntervalSet a ->
  IntervalSet a
difference = foldr (flip differenceISI)


---- Queries on interval sets ----


intersectionAll :: (Ord a, Bounded a) => [IntervalSet a] -> IntervalSet a
intersectionAll (x : xs) = foldr intersection x xs


unionAll :: [IntervalSet a] -> IntervalSet a
unionAll = foldr union []


-- Subtract from the first interval set all the remaining interval sets.
differenceAll :: (Ord a, Bounded a) => [IntervalSet a] -> IntervalSet a
differenceAll (x : xs) = foldl difference x xs


---- Boolean Helpers ----
isEmpty :: Ord a => IntervalSet a -> Bool
isEmpty =
  null . removeEmptyIntervals


-- Hint: areDisjoint and isSubset are simpler than areEqual.
-- Use what you have already defined.

-- Two interval sets are disjoint if they do not overlap
--

areDisjoint :: (Ord a) => IntervalSet a -> IntervalSet a -> Bool
areDisjoint is1 is2 = isEmpty (intersection is1 is2)


isSubset :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
isSubset is1 is2 = isEmpty (difference (is1 `union` is2) is1)


areEqual :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
areEqual is1 is2 =
  is1 `isSubset` is2 && is2 `isSubset` is1

---- Boolean Queries ----

areAllDisjoint :: Ord a => [IntervalSet a] -> Bool
areAllDisjoint [xs] = True
areAllDisjoint (x : xs) = areAllDisjoint' x xs
  where
    areAllDisjoint' l [ll] = areDisjoint l ll
    areAllDisjoint' l (ll : ls) =
      areDisjoint l ll && areAllDisjoint' l ls && areAllDisjoint' ll ls


areAllEqual :: (Ord a, Bounded a) => [IntervalSet a] -> Bool
areAllEqual [xs] = True
areAllEqual (x : xs) = areAllEqual' x xs
  where
    areAllEqual' l [ll] = areEqual l ll
    areAllEqual' l (ll : ls) =
      areEqual l ll && areAllEqual' l ls && areAllEqual' ll ls