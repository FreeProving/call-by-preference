{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.InputFile
  ( -- * Effect
    InStream
  , InputFile (OpenIn, CloseIn, InputAll, InputLine, EndOfStream)
    -- * Actions
  , openIn
  , closeIn
  , inputAll
  , inputLine
  , endOfStream
    -- * Handlers
  , runInputFile
  )
where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           System.IO                 (Handle, IOMode (ReadMode), hClose,
                                            hGetContents, hGetLine, hIsEOF,
                                            openFile)

import           Control.Prog.Class        (HInvariant (hinvmap),
                                            SigFunctor (sigmap),
                                            Syntax (handle), hmap')
import           Control.Prog.Effect.Embed (Embed, embed)
import           Control.Prog.Prog         (Prog (Op, Var))
import           Control.Prog.Signature    ((:+:) (Inl, Inr), (:<:), inject)

------------
-- Effect --
------------

newtype InStream = InStream { inHandle :: Handle }

data InputFile m a where
  OpenIn      :: FilePath -> (InStream -> m a) -> InputFile m a
  CloseIn     :: InStream -> m a               -> InputFile m a
  InputAll    :: InStream -> (String -> m a)   -> InputFile m a
  InputLine   :: InStream -> (String -> m a)   -> InputFile m a
  EndOfStream :: InStream -> (Bool -> m a)     -> InputFile m a

instance SigFunctor InputFile where
  sigmap f (OpenIn      filename k ) = OpenIn      filename (f . k)
  sigmap f (CloseIn     inStream mx) = CloseIn     inStream (f mx)
  sigmap f (InputAll    inStream k ) = InputAll    inStream (f . k)
  sigmap f (InputLine   inStream k ) = InputLine   inStream (f . k)
  sigmap f (EndOfStream inStream k ) = EndOfStream inStream (f . k)

instance HInvariant InputFile where
  hinvmap t _ (OpenIn      filename k ) = OpenIn      filename (t . k)
  hinvmap t _ (CloseIn     inStream mx) = CloseIn     inStream (t mx)
  hinvmap t _ (InputAll    inStream k ) = InputAll    inStream (t . k)
  hinvmap t _ (InputLine   inStream k ) = InputLine   inStream (t . k)
  hinvmap t _ (EndOfStream inStream k ) = EndOfStream inStream (t . k)

instance Syntax InputFile where
  handle c hdl (OpenIn filename k) =
    let ret z = fmap (const z) c in OpenIn filename (hdl . ret . k)
  handle c hdl (CloseIn inStream mx) =
    let ret z = fmap (const z) c in CloseIn inStream (hdl (ret mx))
  handle c hdl (InputAll inStream k) =
    let ret z = fmap (const z) c in InputAll inStream (hdl . ret . k)
  handle c hdl (InputLine inStream k) =
    let ret z = fmap (const z) c in InputLine inStream (hdl . ret . k)
  handle c hdl (EndOfStream inStream k) =
    let ret z = fmap (const z) c in EndOfStream inStream (hdl . ret . k)

-------------
-- Actions --
-------------

openIn :: (InputFile :<: sig) => FilePath -> Prog sig InStream
openIn filename = inject (OpenIn filename return)

closeIn :: (InputFile :<: sig) => InStream -> Prog sig ()
closeIn inStream = inject (CloseIn inStream (return ()))

inputAll :: (InputFile :<: sig) => InStream -> Prog sig String
inputAll inStream = inject (InputAll inStream return)

inputLine :: (InputFile :<: sig) => InStream -> Prog sig String
inputLine inStream = inject (InputLine inStream return)

endOfStream :: (InputFile :<: sig) => InStream -> Prog sig Bool
endOfStream inStream = inject (EndOfStream inStream return)

--------------
-- Handlers --
--------------

runInputFile :: forall m a sig. (Syntax sig, MonadIO m, Embed m :<: sig) => Prog (InputFile :+: sig) a -> Prog sig a
runInputFile (Var x)                              = return x
runInputFile (Op (Inl (OpenIn      filename k ))) = embed (liftIO @m (openFile filename ReadMode)) >>= runInputFile @m . k . InStream
runInputFile (Op (Inl (CloseIn     inStream mx))) = embed (liftIO @m (hClose (inHandle inStream))) >> runInputFile @m mx
runInputFile (Op (Inl (InputAll    inStream k ))) = embed (liftIO @m (hGetContents (inHandle inStream))) >>= runInputFile @m . k
runInputFile (Op (Inl (InputLine   inStream k ))) = embed (liftIO @m (hGetLine (inHandle inStream))) >>= runInputFile @m . k
runInputFile (Op (Inl (EndOfStream inStream k ))) = embed (liftIO @m (hIsEOF (inHandle inStream))) >>= runInputFile @m . k
runInputFile (Op (Inr sig                      )) = Op (hmap' (runInputFile @m) sig)
