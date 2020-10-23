{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.State
  ( -- * Effect
    State (Get, Put)
    -- * Actions
  , get
  , gets
  , put
  , modify
    -- * Handlers
  , runState
  , evalState
  , execState
  , runTransaction
  )
where

import           Control.Prog.Class     (HFunctor (hmap), HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data State s m a = Get (s -> m a) | Put s (m a)

instance SigFunctor (State s) where
  sigmap f (Get k   ) = Get (f . k)
  sigmap f (Put s mx) = Put s (f mx)

instance HFunctor (State s) where
  hmap t (Get k   ) = Get (t . k)
  hmap t (Put s mx) = Put s (t mx)

instance HInvariant (State s) where
  hinvmap f _ (Get k)    = Get (f . k)
  hinvmap f _ (Put s mx) = Put s (f mx)

instance Syntax (State s) where
  handle c hdl (Get k) = let ret z = fmap (const z) c in Get (hdl . ret . k)
  handle c hdl (Put s mx) =
    let ret z = fmap (const z) c in Put s (hdl (ret mx))

-------------
-- Actions --
-------------

get :: forall s sig. (State s :<: sig) => Prog sig s
get = inject (Get return)

gets :: forall s a sig. (State s :<: sig) => (s -> a) -> Prog sig a
gets f = fmap f get

put :: forall s sig. (State s :<: sig) => s -> Prog sig ()
put s = inject (Put s (return ()))

modify :: forall s sig. (State s :<: sig) => (s -> s) -> Prog sig ()
modify f = get >>= put . f

--------------
-- Handlers --
--------------

runState :: forall s a sig. (Syntax sig) => s -> Prog (State s :+: sig) a -> Prog sig (s, a)
runState s (Var x                ) = return (s, x)
runState s (Op  (Inl (Get k    ))) = runState s (k s)
runState _ (Op  (Inl (Put s' mx))) = runState s' mx
runState s (Op  (Inr sig        )) = Op (handle (s, ()) (uncurry runState) sig)

execState :: forall s a sig. (Syntax sig) => s -> Prog (State s :+: sig) a -> Prog sig s
execState s mx = fmap fst (runState s mx)

evalState :: forall s a sig. (Syntax sig) => s -> Prog (State s :+: sig) a -> Prog sig a
evalState s mx = fmap snd (runState s mx)

runTransaction  :: forall s a sig. (Syntax sig, State s :<: sig) => Prog (State s :+: sig) a -> Prog sig a
runTransaction mx = do
  s <- get @s
  fmap snd (runTransaction' s mx)

runTransaction' :: forall s a sig. (Syntax sig, State s :<: sig) => s -> Prog (State s :+: sig) a -> Prog sig (s, a)
runTransaction' s (Var x                ) = put @s s >> return (s, x)
runTransaction' s (Op  (Inl (Get k    ))) = runTransaction' s (k s)
runTransaction' _ (Op  (Inl (Put s' mx))) = runTransaction' s' mx
runTransaction' s (Op  (Inr sig        )) = Op (handle (s, ()) (uncurry runTransaction') sig)
