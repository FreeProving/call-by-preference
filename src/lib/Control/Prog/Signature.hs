{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Prog.Signature
  ( -- * Sum Functor
    (:+:) (Inl, Inr)
    -- * Injecting Operations
  , (:<:)(inj, prj)
  , inject
  )
where

import           Control.Prog.Class (HFunctor (hmap), HInvariant (hinvmap),
                                     SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog  (Prog (Op))

-----------------
-- Sum Functor --
-----------------

data (sig1 :+: sig2) (m :: * -> *) a = Inl (sig1 m a) | Inr (sig2 m a)

infixr 0 :+:

instance (SigFunctor sig1, SigFunctor sig2) => SigFunctor (sig1 :+: sig2) where
  sigmap f (Inl sig) = Inl (sigmap f sig)
  sigmap f (Inr sig) = Inr (sigmap f sig)

instance (Syntax sig1, Syntax sig2) => Syntax (sig1 :+: sig2) where
  handle c hdl (Inl sig) = Inl (handle c hdl sig)
  handle c hdl (Inr sig) = Inr (handle c hdl sig)

instance (HFunctor sig1 , HFunctor sig2) => HFunctor (sig1 :+: sig2) where
  hmap t (Inl sig1) = Inl (hmap t sig1)
  hmap t (Inr sig2) = Inr (hmap t sig2)

instance (HInvariant sig1, HInvariant sig2) => HInvariant (sig1 :+: sig2) where
  hinvmap f g (Inl sig) = Inl (hinvmap f g sig)
  hinvmap f g (Inr sig) = Inr (hinvmap f g sig)

--------------------------
-- Injecting Operations --
--------------------------

class (SigFunctor sub, SigFunctor sup) => sub :<: sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance {-# OVERLAPPING #-} (SigFunctor sig1, SigFunctor sig2) => sig1 :<: (sig1 :+: sig2) where
  inj = Inl
  prj (Inl sig) = Just sig
  prj _         = Nothing

instance {-# OVERLAPPABLE #-} (SigFunctor sig1, sig :<: sig2) => sig :<: (sig1 :+: sig2) where
  inj = Inr . inj
  prj (Inr sig) = prj sig
  prj _         = Nothing

inject :: (sub :<: sup) => sub (Prog sup) a -> Prog sup a
inject = Op . inj
