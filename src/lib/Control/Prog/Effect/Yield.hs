{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.Yield where

import           Control.Monad.Extra    (ifM)

import           Control.Prog.Class     (HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Yield v m a = Yield v (m a)

instance SigFunctor (Yield v) where
  sigmap f (Yield v mx) = Yield v (f mx)

instance HInvariant (Yield v) where
  hinvmap t _ (Yield v mx) = Yield v (t mx)

instance Syntax (Yield v) where
  handle c hdl (Yield v mx) =
    let ret z = fmap (const z) c
    in Yield v (hdl (ret mx))

-------------
-- Actions --
-------------

yield :: forall v sig. (Yield v :<: sig) => v -> Prog sig ()
yield v = inject (Yield v (return ()))

--------------
-- Handlers --
--------------

runYield :: forall v a sig. Syntax sig => (v -> Prog sig Bool) -> Prog (Yield v :+: sig) a -> Prog sig (Maybe a)
runYield _ (Var x                ) = return (Just x)
runYield f (Op (Inl (Yield v mx))) = ifM (f v) (runYield f mx) (return Nothing)
runYield f (Op (Inr sig         )) = Op (handle (Just ()) hdl sig)
 where
  hdl :: forall x. Maybe (Prog (Yield v :+: sig) x) -> Prog sig (Maybe x)
  hdl = maybe (return Nothing) (runYield f)
