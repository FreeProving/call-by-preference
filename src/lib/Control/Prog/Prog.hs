module Control.Prog.Prog (Prog(Var, Op)) where

import           Control.Monad      (ap)

import           Control.Prog.Class (SigFunctor (sigmap))

----------------
-- Prog Monad --
----------------

data Prog sig a = Var a | Op (sig (Prog sig) a)

instance (SigFunctor sig) => Functor (Prog sig) where
  fmap f (Var x  ) = Var (f x)
  fmap f (Op  sig) = Op (sigmap (fmap f) sig)

instance (SigFunctor sig) => Applicative (Prog sig) where
  pure  = return
  (<*>) = ap

instance (SigFunctor sig) => Monad (Prog sig) where
  return = Var
  Var x   >>= f = f x
  Op  sig >>= f = Op (sigmap (>>= f) sig)
