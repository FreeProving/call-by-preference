{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.Deferred
  ( -- * Effect
    Deferred(Force)
    -- * Actions
  , force
    -- * Handlers
  , runDeferred
  )
where

import           Control.Prog.Class     (HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle),
                                         hmap')
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Deferred v m a = Force (v -> m a)

instance SigFunctor (Deferred v) where
  sigmap f (Force k) = Force (f . k)

instance HInvariant (Deferred v) where
  hinvmap t _ (Force k) = Force (t . k)

instance Syntax (Deferred v) where
  handle c hdl (Force k) =
    let ret z = fmap (const z) c in Force (hdl . ret . k)

-------------
-- Actions --
-------------

force :: forall v sig. (Deferred v :<: sig) => Prog sig v
force = inject (Force return)

--------------
-- Handlers --
--------------

runDeferred :: forall v a sig. (Syntax sig) => Prog sig v -> Prog (Deferred v :+: sig) a -> Prog sig a
runDeferred _  (Var x              ) = return x
runDeferred mv (Op  (Inl (Force k))) = do { v <- mv; runForced v (k v) }
runDeferred mv (Op  (Inr sig      )) = Op (hmap' (runDeferred mv) sig)

runForced :: forall v a sig. (Syntax sig) => v -> Prog (Deferred v :+: sig) a -> Prog sig a
runForced _ (Var x              ) = return x
runForced v (Op  (Inl (Force k))) = runForced v (k v)
runForced v (Op  (Inr sig      )) = Op (hmap' (runForced v) sig)
