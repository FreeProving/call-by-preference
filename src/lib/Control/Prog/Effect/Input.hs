{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.Input
  ( -- * Effect
    Input (Input)
    -- * Actions
  , input
    -- * Handlers
  , runInputIO
  , hRunInputIO
  , EndOfInput (EndOfInput)
  , runInputFromList
  )
where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           System.IO                 (Handle, hGetLine, stdin)

import           Control.Prog.Class        (HInvariant (hinvmap),
                                            SigFunctor (sigmap),
                                            Syntax (handle), hmap')
import           Control.Prog.Effect.Embed (Embed, embed)
import           Control.Prog.Effect.Error (Error, throw)
import           Control.Prog.Prog         (Prog (Op, Var))
import           Control.Prog.Signature    ((:+:) (Inl, Inr), (:<:), inject)

-------------
-- Efffect --
-------------

data Input m a = Input (String -> m a)

instance SigFunctor Input where
  sigmap f (Input k) = Input (f . k)

instance HInvariant Input where
  hinvmap t _ (Input k) = Input (t . k)

instance Syntax Input where
  handle c hdl (Input k) = let ret z = fmap (const z) c in Input (hdl . ret . k)

-------------
-- Actions --
-------------

input :: (Input :<: sig) => Prog sig String
input = inject (Input return)

--------------
-- Handlers --
--------------

runInputIO :: forall m a sig. (Syntax sig, MonadIO m, Embed m :<: sig) => Prog (Input :+: sig) a -> Prog sig a
runInputIO = hRunInputIO @m stdin

hRunInputIO :: forall m a sig. (Syntax sig, MonadIO m, Embed m :<: sig) => Handle -> Prog (Input :+: sig) a -> Prog sig a
hRunInputIO _ (Var x             ) = return x
hRunInputIO h (Op (Inl (Input k))) = do
  x <- embed @m (liftIO (hGetLine h))
  hRunInputIO @m h (k x)
hRunInputIO h (Op (Inr sig      )) = Op (hmap' (hRunInputIO @m h) sig)

-- | The error that is thrown by 'runInputFromList' if the list is empty.
data EndOfInput = EndOfInput
 deriving (Eq, Show)

runInputFromList :: (Syntax sig, Error EndOfInput :<: sig) => [String] -> Prog (Input :+: sig) a -> Prog sig a
runInputFromList _         (Var x             ) = return x
runInputFromList []        (Op (Inl (Input _))) = throw EndOfInput
runInputFromList (x : xs') (Op (Inl (Input k))) = runInputFromList xs' (k x)
runInputFromList xs        (Op (Inr sig      )) = Op (hmap' (runInputFromList xs) sig)
