{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.Cut
  ( -- * Effect
    Cut (Cutfail, Call)
    -- * Actions
  , cutfail
  , call
  , cut
    -- * Handlers
  , runCut
  )
where

import           Prelude                    hiding (fail)

import           Control.Monad              ((>=>))

import           Control.Prog.Class         (HInvariant (hinvmap),
                                             SigFunctor (sigmap),
                                             Syntax (handle))
import           Control.Prog.Effect.NonDet (NonDet (Choose, Fail), choose,
                                             fail)
import           Control.Prog.Prog          (Prog (Op, Var))
import           Control.Prog.Signature     ((:+:) (Inl, Inr), (:<:) (prj),
                                             inject)

------------
-- Effect --
------------

data Cut m a where
  Cutfail ::                      Cut m a
  Call    :: m x -> (x -> m a) -> Cut m a

instance SigFunctor Cut where
  sigmap _ Cutfail     = Cutfail
  sigmap f (Call mx k) = Call mx (f . k)

instance HInvariant Cut where
  hinvmap _ _ Cutfail     = Cutfail
  hinvmap t _ (Call mx k) = Call (t mx) (t . k)

instance Syntax Cut where
  handle _ _   Cutfail     = Cutfail
  handle c hdl (Call mx k) =
    let ret z = fmap (const z) c
    in  Call (hdl (ret mx)) (hdl . fmap k)

-------------
-- Actions --
-------------

cutfail :: (Cut :<: sig) => Prog sig a
cutfail = inject Cutfail

call :: (Cut :<: sig) => Prog sig a -> Prog sig a
call mx = inject (Call mx return)

cut :: (Cut :<: sig, NonDet :<: sig) => Prog sig ()
cut = choose (return ()) cutfail

--------------
-- Handlers --
--------------

data Result a = Result { backtrack :: Bool, value :: Maybe a }
 deriving Functor

runCut :: forall a sig. (Syntax sig, NonDet :<: sig) => Prog (Cut :+: sig) a -> Prog sig a
runCut = runCut' True >=> maybe fail return . value
 where
  runCut' :: forall x. Bool -> Prog (Cut :+: sig) x -> Prog sig (Result x)
  runCut' b (Var x               ) = return (Result b (Just x))
  runCut' _ (Op (Inl Cutfail    )) = return (Result False Nothing)
  runCut' b (Op (Inl (Call mx k))) = do
    rx <- runCut' b mx
    case (value rx) of
      Nothing -> return (Result b Nothing)
      Just x  -> runCut' (backtrack rx) (k x)
  runCut' b (Op (Inr sig        )) = case prj sig of
    Just Fail           -> return (Result b Nothing)
    Just (Choose ml mr) -> do
      rl <- runCut' b ml
      if backtrack rl
        then choose (return rl { backtrack = False }) (runCut' b mr)
        else return rl
    Nothing             -> Op (handle (Result b (Just ())) hdl sig)

  hdl :: forall x. Result (Prog (Cut :+: sig) x) -> Prog sig (Result x)
  hdl (Result b (Just mx)) = runCut' b mx
  hdl (Result b Nothing  ) = return (Result b Nothing)
