{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.Trace
  ( -- * Effect
    Trace (Trace)
    -- * Actions
  , trace
    -- * Handlers
  , runTrace
    -- * Reinterpreters
  , traceToWriter
  )
where

import           Control.Prog.Class         (HInvariant (hinvmap),
                                             SigFunctor (sigmap),
                                             Syntax (handle), hmap')
import           Control.Prog.Effect.Writer (Writer, runWriter, tell)
import           Control.Prog.Prog          (Prog (Op, Var))
import           Control.Prog.Signature     ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

data Trace m a = Trace String (m a)

instance SigFunctor Trace where
  sigmap f (Trace msg mx) = Trace msg (f mx)

instance HInvariant Trace where
  hinvmap f _ (Trace msg mx) = Trace msg (f mx)

instance Syntax Trace where
  handle c hdl (Trace msg mx) =
    let ret z = fmap (const z) c in Trace msg (hdl (ret mx))

-------------
-- Actions --
-------------

trace :: forall a sig. (Trace :<: sig) => String -> Prog sig a -> Prog sig a
trace msg mx = inject (Trace msg mx)

--------------
-- Handlers --
--------------

runTrace :: forall a sig. (Syntax sig) => Prog (Trace :+: sig) a -> Prog sig ([String], a)
runTrace = runWriter . traceToWriter

--------------------
-- Reinterpreters --
--------------------

traceToWriter
  :: forall a sig
   . (Syntax sig)
  => Prog (Trace :+: sig) a
  -> Prog (Writer [String] :+: sig) a
traceToWriter (Var a                   ) = return a
traceToWriter (Op  (Inl (Trace msg mx))) = tell [msg] >> traceToWriter mx
traceToWriter (Op  (Inr sig           )) = Op (Inr (hmap' traceToWriter sig))
