{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Control.Prog.Class
  ( -- * Natural Transformations
    type (-->)
    -- * Signatures
   , SigFunctor (sigmap)
    -- * Handlers for Higher-Order Effects
   , Syntax (handle)
   , hmap'
    -- * Higher-Order Functors
   , HFunctor (hmap)
    -- * Invariant Higher-Order Functors
   , HInvariant (hinvmap)
  ) where

import           Data.Functor.Identity (Identity (Identity), runIdentity)

-----------------------------
-- Natural Transformations --
-----------------------------

type f --> g = forall x. f x -> g x

----------------
-- Signatures --
----------------

class SigFunctor sig where
  sigmap :: (Monad m) => (m a -> m b) -> sig m a -> sig m b

---------------------------------------
-- Handlers for Higher-Order Effects --
---------------------------------------

class SigFunctor sig => Syntax sig where
  handle :: (Monad m, Monad n, Functor c)
         => c ()
         -> (forall x. c (m x) -> n (c x))
         -> sig m a
         -> sig n (c a)

-- | Version of 'hmap' for signatures with a 'Syntax' but no 'HFunctor'
--   instance.
hmap'
  :: (Syntax sig, Monad m, Monad n)
  => (forall x. m x -> n x)
  -> sig m a
  -> sig n a
hmap' hdl
  = sigmap (fmap runIdentity)
  . handle (Identity ()) (fmap Identity . hdl . runIdentity)

---------------------------
-- Higher-Order Functors --
---------------------------

class HFunctor sig where
  hmap :: (Functor f, Functor g) => (f --> g) -> sig f --> sig g

-------------------------------------
-- Invariant Higher-Order Functors --
-------------------------------------

class HInvariant h where
  hinvmap :: (Functor f, Functor g) => (f --> g) -> (g --> f) -> h f --> h g
