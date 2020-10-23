{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.Error
  ( -- * Effect
    Error(Throw, Catch)
    -- * Actions
  , throw
  , catch
  , try
    -- * Handlers
  , runError
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

data Error e m a where
  Throw :: e                               -> Error e m a
  Catch :: m x -> (e -> m x) -> (x -> m a) -> Error e m a

instance SigFunctor (Error e) where
  sigmap _ (Throw e     ) = Throw e
  sigmap f (Catch mx h k) = Catch mx h (f . k)

instance HInvariant (Error e) where
  hinvmap _ _ (Throw e     ) = Throw e
  hinvmap f _ (Catch mx h k) = Catch (f mx) (f . h) (f . k)

instance Syntax (Error e) where
  handle _ _   (Throw e)      = Throw e
  handle c hdl (Catch mx h k) =
    let ret z = fmap (const z) c
    in Catch (hdl (ret mx)) (hdl . ret . h) (hdl . fmap k)

-------------
-- Actions --
-------------

throw :: forall e a sig. (Error e :<: sig) => e -> Prog sig a
throw e = inject (Throw e)

catch :: forall e x sig. (Error e :<: sig) => Prog sig x -> (e -> Prog sig x) -> Prog sig x
catch mx h = inject (Catch mx h return)

try :: forall e x sig. (Error e :<: sig) => Prog sig x -> Prog sig (Either e x)
try mx = catch (fmap Right mx) (return . Left)

-------------
-- Handler --
-------------

runError :: forall e a sig. Syntax sig => Prog (Error e :+: sig) a -> Prog sig (Either e a)
runError (Var            x       ) = return (Right x)
runError (Op (Inl (Throw x     ))) = return (Left x)
runError (Op (Inl (Catch mx h k))) = do
  ex <- runError mx
  case ex of
    Left  e -> runError (h e) >>= either (return . Left) (runError . k)
    Right x -> runError (k x)
runError (Op (Inr sig)) = Op (handle (Right ()) hdl sig)
 where
  hdl :: Syntax sig
      => Either e (Prog (Error e :+: sig) x)
      -> Prog sig (Either e x)
  hdl = either (return . Left) runError
