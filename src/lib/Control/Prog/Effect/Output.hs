{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.Output
  ( -- * Effect
    Output(Output)
    -- * Actions
  , output
    -- * Handlers
  , runOutputIO
  , hRunOutputIO
  , runOutputToList
  , discardOutput
  )
where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           System.IO                 (Handle, hPutStrLn, stdout)

import           Control.Prog.Class        (HInvariant (hinvmap),
                                            SigFunctor (sigmap),
                                            Syntax (handle), hmap')
import           Control.Prog.Effect.Embed (Embed, embed)
import           Control.Prog.Prog         (Prog (Op, Var))
import           Control.Prog.Signature    ((:+:) (Inl, Inr), (:<:), inject)

-------------
-- Efffect --
-------------

data Output m a = Output String (m a)

instance SigFunctor Output where
  sigmap f (Output x mx) = Output x (f mx)

instance HInvariant Output where
  hinvmap f _ (Output x mx) = Output x (f mx)

instance Syntax Output where
  handle c hdl (Output x mx) =
    let ret z = fmap (const z) c in Output x (hdl (ret mx))

-------------
-- Actions --
-------------

output :: (Output :<: sig) => String -> Prog sig ()
output x = inject (Output x (return ()))

--------------
-- Handlers --
--------------

runOutputIO :: forall m a sig. (Syntax sig, MonadIO m, Embed m :<: sig) => Prog (Output :+: sig) a -> Prog sig a
runOutputIO = hRunOutputIO @m stdout

hRunOutputIO :: forall m a sig. (Syntax sig, MonadIO m, Embed m :<: sig) => Handle -> Prog (Output :+: sig) a -> Prog sig a
hRunOutputIO _ (Var x                 ) = return x
hRunOutputIO h (Op (Inl (Output x mx))) = do
  embed @m (liftIO (hPutStrLn h x))
  hRunOutputIO @m h mx
hRunOutputIO h (Op (Inr sig          )) = Op (hmap' (hRunOutputIO @m h) sig)

runOutputToList :: Syntax sig => Prog (Output :+: sig) a -> Prog sig ([String], a)
runOutputToList (Var x                 ) = return ([], x)
runOutputToList (Op (Inl (Output x mx))) = do
  (xs, v) <- runOutputToList mx
  return (x : xs, v)
runOutputToList (Op (Inr sig          )) =
  Op (handle ([], ()) (\(_, mx) -> runOutputToList mx) sig)

discardOutput :: Syntax sig => Prog (Output :+: sig) a -> Prog sig a
discardOutput (Var x                 ) = return x
discardOutput (Op (Inl (Output _ mx))) = discardOutput mx
discardOutput (Op (Inr sig          )) = Op (hmap' discardOutput sig)
