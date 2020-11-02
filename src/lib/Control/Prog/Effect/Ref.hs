{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}

module Control.Prog.Effect.Ref
  ( -- * Effect
    Ref (NewRef, ReadRef, WriteRef)
    -- * Actions
  , newRef
  , readRef
  , writeRef
    -- * Handlers
  , runIORef
  , runSTRef
  )
where

import           Control.Monad.ST          (ST)
import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           Data.STRef                (STRef, newSTRef, readSTRef,
                                            writeSTRef)
import           Data.Typeable             (Typeable)

import           Control.Prog.Class        (HInvariant (hinvmap),
                                            SigFunctor (sigmap),
                                            Syntax (handle), hmap')
import           Control.Prog.Effect.Embed (Embed, embed)
import           Control.Prog.Prog         (Prog (Op, Var))
import           Control.Prog.Signature    ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Ref loc m a
  = forall dat. (Typeable dat) => NewRef dat (loc dat -> m a)
  | forall dat. (Typeable dat) => ReadRef (loc dat) (dat -> m a)
  | forall dat. (Typeable dat) => WriteRef (loc dat) dat (m a)

instance SigFunctor (Ref loc) where
  sigmap f (NewRef dat k)       = NewRef dat (f . k)
  sigmap f (ReadRef loc k)      = ReadRef loc (f . k)
  sigmap f (WriteRef loc dat a) = WriteRef loc dat (f a)

instance HInvariant (Ref loc) where
  hinvmap f _ (NewRef dat k)        = NewRef dat (f . k)
  hinvmap f _ (ReadRef loc k)       = ReadRef loc (f . k)
  hinvmap f _ (WriteRef loc dat mx) = WriteRef loc dat (f mx)

instance Syntax (Ref loc) where
  handle c hdl (NewRef dat k)        = let ret z = fmap (const z) c in NewRef dat (hdl . ret . k)
  handle c hdl (ReadRef loc k)       = let ret z = fmap (const z) c in ReadRef loc (hdl . ret . k)
  handle c hdl (WriteRef loc dat mx) = let ret z = fmap (const z) c in WriteRef loc dat (hdl (ret mx))

-------------
-- Actions --
-------------

newRef :: forall loc dat sig. (Ref loc :<: sig, Typeable dat) => dat -> Prog sig (loc dat)
newRef dat = inject (NewRef dat return)

readRef :: forall loc dat sig. (Ref loc :<: sig, Typeable dat) => loc dat -> Prog sig dat
readRef loc = inject (ReadRef loc return)

writeRef :: forall loc dat sig. (Ref loc :<: sig, Typeable dat) => loc dat -> dat -> Prog sig ()
writeRef loc dat = inject (WriteRef loc dat (return ()))

--------------
-- Handlers --
--------------

runIORef :: forall a sig. (Syntax sig, Embed IO :<: sig) => Prog (Ref IORef :+: sig) a -> Prog sig a
runIORef (Var x)                       = return x
runIORef (Op (Inl (NewRef b k)))       = do
  r <- embed (newIORef b)
  runIORef (k r)
runIORef (Op (Inl (ReadRef ref k)))    = do
  v <- embed (readIORef ref)
  runIORef (k v)
runIORef (Op (Inl (WriteRef ref b v))) = do
  embed (writeIORef ref b)
  runIORef v
runIORef (Op (Inr sig)) = Op (hmap' runIORef sig)

runSTRef :: forall s a sig. (Syntax sig, Embed (ST s) :<: sig) => Prog (Ref (STRef s) :+: sig) a -> Prog sig a
runSTRef (Var x)                       = return x
runSTRef (Op (Inl (NewRef b k)))       = do
  r <- embed (newSTRef b)
  runSTRef (k r)
runSTRef (Op (Inl (ReadRef ref k)))    = do
  v <- embed (readSTRef ref)
  runSTRef (k v)
runSTRef (Op (Inl (WriteRef ref b v))) = do
  embed (writeSTRef ref b)
  runSTRef v
runSTRef (Op (Inr sig)) = Op (hmap' runSTRef sig)
