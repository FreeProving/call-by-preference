{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

-- | This module contains monadicly lifted versions of the 'Ref' effect's
--   actions.
module Control.Prog.Example.ML.Ref
 ( newRef
 , readRef
 , writeRef
 )
where

import           Data.Typeable           (Typeable)

import           Control.Prog            ((:<:), Prog)
import           Control.Prog.Effect.Ref (Ref)
import qualified Control.Prog.Effect.Ref as Ref

newRef :: (Ref loc :<: sig, Typeable dat) => Prog sig dat -> Prog sig (loc dat)
newRef mx = mx >>= Ref.newRef

readRef :: (Ref loc :<: sig, Typeable dat) => Prog sig (loc dat) -> Prog sig dat
readRef ml = ml >>= Ref.readRef

writeRef :: (Ref loc :<: sig, Typeable dat) => Prog sig (loc dat) -> Prog sig dat -> Prog sig ()
writeRef ml mx = ml >>= \l -> mx >>= Ref.writeRef l
