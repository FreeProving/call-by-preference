{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.Embed
  ( -- * Effect
    Embed(Embed)
    -- * Actions
  , embed
    -- * Handlers
  , runM
  )
where

import           Control.Prog.Class       (HInvariant (hinvmap),
                                           SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Effect.None (None)
import           Control.Prog.Prog        (Prog (Op, Var))
import           Control.Prog.Signature   ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Embed m n a where
  Embed :: m b -> (b -> n a) -> Embed m n a

instance SigFunctor (Embed e) where
  sigmap f (Embed mx k) = Embed mx (f . k)

instance HInvariant (Embed e) where
  hinvmap f _ (Embed mx k) = Embed mx (f . k)

instance Syntax (Embed e) where
  handle c hdl (Embed mx k) =
    let ret z = fmap (const z) c in Embed mx (hdl . ret . k)

-------------
-- Actions --
-------------

embed :: forall m a sig. (Embed m :<: sig) => m a -> Prog sig a
embed mx = inject (Embed mx return)

--------------
-- Handlers --
--------------

runM :: forall m a. (Monad m) => Prog (Embed m :+: None) a -> m a
runM (Var x                 ) = return x
runM (Op  (Inl (Embed mx k))) = mx >>= runM . k
runM (Op  (Inr sig         )) = case sig of {}
