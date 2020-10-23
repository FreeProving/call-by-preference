{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.Fail
  ( -- * Effect
    Fail(Fail)
    -- * Actions
  , fail
    -- * Handlers
  , runFail
  )
where

import           Prelude                hiding (fail)

import           Control.Prog.Class     (HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Fail (m :: * -> *) a = Fail

instance SigFunctor Fail where
  sigmap _ Fail = Fail

instance HInvariant Fail where
  hinvmap _ _ Fail = Fail

instance Syntax Fail where
  handle _ _ Fail = Fail

-------------
-- Actions --
-------------

fail :: forall a sig. (Fail :<: sig) => Prog sig a
fail = inject Fail

--------------
-- Handlers --
--------------

runFail :: forall a sig. (Syntax sig) => Prog (Fail :+: sig) a -> Prog sig (Maybe a)
runFail (Var x         ) = return (Just x)
runFail (Op  (Inl Fail)) = return Nothing
runFail (Op  (Inr sig )) = Op (handle (Just ()) hdl sig)
 where
  hdl :: Maybe (Prog (Fail :+: sig) x) -> Prog sig (Maybe x)
  hdl Nothing   = return Nothing
  hdl (Just fx) = runFail fx
