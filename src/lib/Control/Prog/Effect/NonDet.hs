{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.NonDet
  ( -- * Effect
    NonDet(Choose, Fail)
    -- * Actions
  , choose
  , fail
    -- * Handlers
  , runNonDet
  )
where

import           Prelude                hiding (fail)

import           Control.Monad          (liftM2)
import           Control.Monad.Extra    (concatMapM)

import           Control.Prog.Class     (HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data NonDet (m :: * -> *) a = Choose (m a) (m a) | Fail

instance SigFunctor NonDet where
  sigmap f (Choose mx my) = Choose (f mx) (f my)
  sigmap _ Fail           = Fail

instance HInvariant NonDet where
  hinvmap f _ (Choose mx my) = Choose (f mx) (f my)
  hinvmap _ _ Fail           = Fail

instance Syntax NonDet where
  handle c hdl (Choose mx my) =
    let ret z = fmap (const z) c in Choose (hdl (ret mx)) (hdl (ret my))
  handle _ _ Fail = Fail


-------------
-- Actions --
-------------

choose :: forall a sig. (NonDet :<: sig) => Prog sig a -> Prog sig a -> Prog sig a
choose fx fy = inject (Choose fx fy)

fail :: forall a sig. (NonDet :<: sig) => Prog sig a
fail = inject Fail

--------------
-- Handlers --
--------------

runNonDet :: forall a sig. (Syntax sig) => Prog (NonDet :+: sig) a -> Prog sig [a]
runNonDet (Var x                   ) = return [x]
runNonDet (Op  (Inl (Choose mx my))) = liftM2 (++) (runNonDet mx) (runNonDet my)
runNonDet (Op  (Inl Fail          )) = return []
runNonDet (Op  (Inr sig           )) = Op (handle [()] hdl sig)
 where
  hdl :: (Syntax sig) => [Prog (NonDet :+: sig) x] -> Prog sig [x]
  hdl = concatMapM runNonDet
