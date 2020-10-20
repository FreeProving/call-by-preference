{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.Writer
  ( -- * Effect
    Writer (Tell)
    -- * Actions
  , tell
    -- * Handlers
  , runWriter
  , execWriter
  )
where

import           Control.Prog.Class     (HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Writer w m a = Tell w (m a)

instance SigFunctor (Writer w) where
  sigmap f (Tell w mx) = Tell w (f mx)

instance HInvariant (Writer w) where
  hinvmap f _ (Tell w mx) = Tell w (f mx)

instance Syntax (Writer w) where
  handle c hdl (Tell w mx) =
    let ret z = fmap (const z) c in Tell w (hdl (ret mx))

-------------
-- Actions --
-------------

tell :: forall w sig. (Writer w :<: sig) => w -> Prog sig ()
tell w = inject (Tell w (return ()))

--------------
-- Handlers --
--------------

runWriter
  :: forall w a sig
   . (Syntax sig, Monoid w)
  => Prog (Writer w :+: sig) a -> Prog sig (w, a)
runWriter = runWriter' mempty
 where
  runWriter'
    :: (Syntax sig, Monoid w)
    => w -> Prog (Writer w :+: sig) a -> Prog sig (w, a)
  runWriter' w (Var a                 ) = return (w, a)
  runWriter' w (Op  (Inl (Tell w' mx))) = runWriter' (w `mappend` w') mx
  runWriter' w (Op  (Inr sig         )) =
    Op (handle (w, ()) (uncurry runWriter') sig)

execWriter
  :: forall w a sig
   . (Syntax sig, Monoid w)
  => Prog (Writer w :+: sig) a -> Prog sig w
execWriter mx = fmap fst (runWriter mx)
