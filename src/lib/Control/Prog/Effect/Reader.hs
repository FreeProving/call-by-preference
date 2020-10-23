{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.Reader
  ( -- * Effect
    Reader(Ask, Local)
    -- * Actions
  , ask
  , asks
  , local
    -- * Handlers
  , runReader
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

data Reader r m a where
  Ask   ::                    (r -> m a) -> Reader r m a
  Local :: (r -> r) -> m x -> (x -> m a) -> Reader r m a

instance SigFunctor (Reader r) where
  sigmap f (Ask        k) = Ask (f . k)
  sigmap f (Local g mx k) = Local g mx (f . k)

instance HInvariant (Reader r) where
  hinvmap f _ (Ask        k) = Ask (f . k)
  hinvmap f _ (Local g mx k) = Local g (f mx) (f . k)

instance Syntax (Reader r) where
  handle c hdl (Ask        k) =
    let ret z = fmap (const z) c in Ask (hdl . ret . k)
  handle c hdl (Local g mx k) =
    let ret z = fmap (const z) c in Local g (hdl (ret mx)) (hdl . fmap k)

-------------
-- Actions --
-------------

ask :: forall r sig. (Reader r :<: sig) => Prog sig r
ask = inject (Ask return)

asks :: forall r a sig. (Reader r :<: sig) => (r -> a) -> Prog sig a
asks f = fmap f ask

local :: forall r a sig. (Reader r :<: sig) => (r -> r) -> Prog sig a -> Prog sig a
local g mx = inject (Local g mx return)

--------------
-- Handlers --
--------------

runReader :: forall r a sig. (Syntax sig) => r -> Prog (Reader r :+: sig) a -> Prog sig a
runReader _ (Var a                   ) = return a
runReader r (Op  (Inl (Ask        k))) = runReader r (k r)
runReader r (Op  (Inl (Local g mx k))) = do
  x <- runReader (g r) mx
  runReader r (k x)
runReader r (Op  (Inr sig           )) = Op (hmap' (runReader r) sig)
