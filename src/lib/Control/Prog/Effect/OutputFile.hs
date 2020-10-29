{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.OutputFile
  ( -- * Effect
    OutputFile (OpenOut, OpenAppend, CloseOut, Output, FlushOut)
    -- * Actions
  , openOut
  , openAppend
  , closeOut
  , output
  , flushOut
    -- * Handlers
  , runOutputFile
  )
where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           System.IO                 (Handle,
                                            IOMode (AppendMode, WriteMode),
                                            hClose, hFlush, hPutStr, openFile)

import           Control.Prog.Class        (HInvariant (hinvmap),
                                            SigFunctor (sigmap),
                                            Syntax (handle), hmap')
import           Control.Prog.Effect.Embed (Embed, embed)
import           Control.Prog.Prog         (Prog (Op, Var))
import           Control.Prog.Signature    ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

newtype OutStream = OutStream { outHandle :: Handle }

data OutputFile m a where
  OpenOut    :: FilePath            -> (OutStream -> m a) -> OutputFile m a
  OpenAppend :: FilePath            -> (OutStream -> m a) -> OutputFile m a
  CloseOut   :: OutStream           -> m a                -> OutputFile m a
  Output     :: OutStream -> String -> m a                -> OutputFile m a
  FlushOut   :: OutStream           -> m a                -> OutputFile m a

instance SigFunctor OutputFile where
  sigmap f (OpenOut    filename      k ) = OpenOut    filename      (f . k)
  sigmap f (OpenAppend filename      k ) = OpenAppend filename      (f . k)
  sigmap f (CloseOut   outStream     mx) = CloseOut   outStream     (f mx)
  sigmap f (Output     outStream str mx) = Output     outStream str (f mx)
  sigmap f (FlushOut   outStream     mx) = FlushOut   outStream     (f mx)

instance HInvariant OutputFile where
  hinvmap t _ (OpenOut    filename      k ) = OpenOut    filename      (t . k)
  hinvmap t _ (OpenAppend filename      k ) = OpenAppend filename      (t . k)
  hinvmap t _ (CloseOut   outStream     mx) = CloseOut   outStream     (t mx)
  hinvmap t _ (Output     outStream str mx) = Output     outStream str (t mx)
  hinvmap t _ (FlushOut   outStream     mx) = FlushOut   outStream     (t mx)

instance Syntax OutputFile where
  handle c hdl (OpenOut    filename      k ) = let ret z = fmap (const z) c in OpenOut    filename      (hdl . ret . k)
  handle c hdl (OpenAppend filename      k ) = let ret z = fmap (const z) c in OpenAppend filename      (hdl . ret . k)
  handle c hdl (CloseOut   outStream     mx) = let ret z = fmap (const z) c in CloseOut   outStream     (hdl (ret mx))
  handle c hdl (Output     outStream str mx) = let ret z = fmap (const z) c in Output     outStream str (hdl (ret mx))
  handle c hdl (FlushOut   outStream     mx) = let ret z = fmap (const z) c in FlushOut   outStream     (hdl (ret mx))

-------------
-- Actions --
-------------

openOut :: (OutputFile :<: sig) => FilePath -> Prog sig OutStream
openOut filename = inject (OpenOut filename return)

openAppend :: (OutputFile :<: sig) => FilePath -> Prog sig OutStream
openAppend filename = inject (OpenAppend filename return)

closeOut :: (OutputFile :<: sig) => OutStream -> Prog sig ()
closeOut outStream = inject (CloseOut outStream (return ()))

output :: (OutputFile :<: sig) => OutStream -> String -> Prog sig ()
output outStream str = inject (Output outStream str (return ()))

flushOut :: (OutputFile :<: sig) => OutStream -> Prog sig ()
flushOut outStream = inject (FlushOut outStream (return ()))

--------------
-- Handlers --
--------------

runOutputFile :: forall m a sig. (Syntax sig, MonadIO m, Embed m :<: sig) => Prog (OutputFile :+: sig) a -> Prog sig a
runOutputFile (Var x)                                  = return x
runOutputFile (Op (Inl (OpenOut    filename      k ))) = embed (liftIO @m (openFile filename WriteMode)) >>= runOutputFile @m . k . OutStream
runOutputFile (Op (Inl (OpenAppend filename      k ))) = embed (liftIO @m (openFile filename AppendMode)) >>= runOutputFile @m . k . OutStream
runOutputFile (Op (Inl (CloseOut   outStream     mx))) = embed (liftIO @m (hClose (outHandle outStream))) >> runOutputFile @m mx
runOutputFile (Op (Inl (Output     outStream str mx))) = embed (liftIO @m (hPutStr (outHandle outStream) str)) >> runOutputFile @m mx
runOutputFile (Op (Inl (FlushOut   outStream     mx))) = embed (liftIO @m (hFlush (outHandle outStream))) >> runOutputFile @m mx
runOutputFile (Op (Inr sig                          )) = Op (hmap' (runOutputFile @m) sig)
