module State where


newtype State s a =
  State { runState :: s -> (a,s) }


instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap g sa = State $ \s0 ->
    let
      (a,s1) = runState sa s0
    in
      (g a, s1)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  sab <*> sa = State $ \s0 ->
    let
      (f, s1) = runState sab s0
      (a, s2) = runState sa s1
    in
      (f a, s2)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  sa >>= f = State $ \s0 ->
    let
      (a, s1) = runState sa s0
      (b, s2) = runState (f a) s1
    in
      (b, s2)

get       :: State s s                  -- get state out
put       :: s -> State s ()            -- set "current" state
modify    :: (s -> s) -> State s ()     -- modify the state
evalState :: State s a -> s -> a        -- run and return final value
execState :: State s a -> s -> s        -- run and return final state

get            = State $ \s -> (s, s)
put s'         = State $ \s -> ((), s')
modify f       = State $ \s -> ((), f s)
evalState sa s = fst $ runState sa s
execState sa s = snd $ runState sa s
