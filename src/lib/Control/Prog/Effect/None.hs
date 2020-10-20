{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE KindSignatures #-}

module Control.Prog.Effect.None where

import           Control.Prog.Class   (HFunctor (hmap), HInvariant (hinvmap), SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog             (Prog (Op, Var))

------------
-- Effect --
------------

data None (m :: * -> *) a

instance SigFunctor None where
  sigmap _ x = case x of {}

instance HFunctor None where
  hmap _ mx = case mx of {}

instance HInvariant None where
  hinvmap _ _ mx = case mx of {}

instance Syntax None where
  handle _ _ x = case x of {}

--------------
-- Handlers --
--------------

run :: Prog None a -> a
run (Var x  ) = x
run (Op  sig) = case sig of {}
