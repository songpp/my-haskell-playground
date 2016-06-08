{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module StateMonad where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Functor.Identity


runState :: State s a -> s -> (a, s)
runState st = runIdentity . runStateT st

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

evaluateState :: State s a -> s -> a
-- evaluateState = evalState
evaluateState st = fst . runState st

get :: State s s
get = state $ \s -> (s, s)

put :: s -> State s ()
put s = state $ \s -> ((), s)

newtype StateT s m a = StateT { runStateT :: s ->  m (a, s) }

instance MonadTrans (StateT s) where
  -- lift :: m a -> StateT s m a
  lift m = StateT $ \s -> m >>= \ a -> return (a, s)

instance (Functor m) => Functor (StateT s m) where
  -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f st = StateT $ \s ->
    let ma = runStateT st s in fmap (\ ~(a, s) -> (f a, s) ) ma

type State s = StateT s Identity


{--
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State st) = State $ \s ->
    let (a, s') = st s in (f a, s')


instance Applicative (State s) where
  pure = return
  -- State s (a -> b) -> State s a -> State s b
  (State s1) <*> (State s2) = State $ \s ->
    let (f, s')  = s1 s
        (a, s'') = s2 s'
     in (f a, s'')

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State st) >>= f = State $ \s ->
    let (a, s') = st s in runState (f a) s'
--}
