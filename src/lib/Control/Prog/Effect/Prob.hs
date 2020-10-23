{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.Prob
  ( -- * Effect
    Prob
    -- * Actions
  , choice
    -- * Handlers
  , runProb
    -- * Semantics
  , Probability
  , Dist(Dist)
  , fromDist
  , normalize
  , pick
  )
where

import           Control.Monad          (ap, join, liftM2)
import           Data.Function          (on)
import           Data.List              (intercalate)

import           Control.Prog.Class     (HInvariant (hinvmap),
                                         SigFunctor (sigmap), Syntax (handle))
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inl, Inr), (:<:), inject)
import           Control.Prog.Util.List (groupBy)

------------
-- Effect --
------------

data Prob m a = Choice Probability (m a) (m a)

instance SigFunctor Prob where
  sigmap f (Choice pr mx my) = Choice pr (f mx) (f my)

instance HInvariant Prob where
  hinvmap f _ (Choice pr mx my) = Choice pr (f mx) (f my)

instance Syntax Prob where
  handle c hdl (Choice pr mx my) =
    let ret z = fmap (const z) c in Choice pr (hdl (ret mx)) (hdl (ret my))

-------------
-- Actions --
-------------

choice
  :: forall a sig
   . (Prob :<: sig)
  => Probability
  -> Prog sig a
  -> Prog sig a
  -> Prog sig a
choice pr fx fy = inject (Choice pr fx fy)

--------------
-- Handlers --
--------------

runProb :: forall a sig. (Syntax sig) => Prog (Prob :+: sig) a -> Prog sig (Dist a)
runProb (Var x)                       = return (return x)
runProb (Op (Inl (Choice pr px py)))  = liftM2 (pick pr) (runProb px) (runProb py)
runProb (Op (Inr sig))                = Op (handle (return ()) hdl sig)
 where
  hdl :: forall x. Dist (Prog (Prob :+: sig) x) -> Prog sig (Dist x)
  hdl (Dist ps) = do
    ds <- mapM (\(pd, p) -> fmap (\x -> (x,p)) (runProb pd)) ps
    return (join (Dist ds))

---------------
-- Semantics --
---------------

type Probability = Float

newtype Dist a = Dist {fromDist :: [(a,Probability)]}
  deriving Functor

instance Applicative Dist where
  pure = return
  (<*>) = ap

instance Monad Dist where
  return x = Dist [(x,1)]
  Dist d >>= f = Dist [(y,q*p) | (x,p) <- d, (y,q) <- fromDist (f x)]

instance (Show a, Eq a) => Show (Dist a) where
  show = brackets . intercalate ", " . map (uncurry prettyLine) . normalize . fromDist
    where
      brackets str = "[" ++ str ++ "]"
      prettyLine v p = show v ++ " -> " ++ show p

normalize :: (Eq a) => [(a, Probability)] -> [(a, Probability)]
normalize dist = map combineGroup xs
 where
  xs = groupBy (on (==) fst) dist
  combineGroup g = (fst (head g), sum (map snd g))

pick :: Probability -> Dist a -> Dist a -> Dist a
pick p dx dy = join (Dist [(dx, p), (dy, 1 - p)])
