{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
runDeferred pv = fmap snd . runDeferred' Nothing
 where
  runDeferred' :: forall x. Maybe v -> Prog (Deferred v :+: sig) x -> Prog sig (Maybe v, x)
  runDeferred' mv (Var x               ) = return (mv, x)
  runDeferred' mv (Op  (Inl (Force k)))  = case mv of
    Nothing -> do { v <- pv; runDeferred' (Just v) (k v) }
    Just v  -> runDeferred' (Just v) (k v)
  runDeferred' mv (Op  (Inr sig       )) = Op (handle (mv, ()) (uncurry runDeferred') sig)
