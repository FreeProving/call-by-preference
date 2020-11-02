{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Example.ML
  ( ML
    -- * Factorial
  , fac
    -- $math
  , minus
  , mult
  , add
  , lt
  , le
  , eq
  , neq
  , ge
  , gt
    -- * File IO
    -- $poem
  , writePoem
  , readPoem
    -- * References
  , decrementToZero
    -- * Tests
  , testMLExamples
  ) where

import           Prelude                        hiding (fail, fst, repeat, snd,
                                                 take, undefined)

import           Control.Monad                  (liftM2)
import           Control.Monad.Extra            (ifM)
import           Control.Monad.Loops            (whileM_)
import           Data.IORef                     (IORef)
import           Data.Typeable                  (Typeable)
import           System.Directory               (removeFile)
import           Test.Hspec                     (Spec, after_, context,
                                                 describe, it, shouldBe)

import           Control.Prog
import           Control.Prog.Effect.InputFile  (InputFile, runInputFile)
import           Control.Prog.Effect.OutputFile (OutputFile, runOutputFile)
import           Control.Prog.Effect.Ref        (Ref, newRef, readRef, runIORef,
                                                 writeRef)
import qualified Control.Prog.Example.ML.TextIO as TextIO

-- | Tag for 'Let' effects that should be handled like in ML-like languages
--   (i.e., with call-by-value semantics).
data ML

---------------
-- Factorial --
---------------

-- | Computes the factorial of the given computation's return value.
--
--   The argument is bound via 'let_' such that call-by-value semantics are
--   archived.
fac :: (Let ML :<: sig) => Prog sig Int -> Prog sig Int
fac pn = do
  pn' <- let_ @ML pn
  n <- pn'
  if n <= 0 then return 0
            else mult pn' (fac (minus pn' (return 1)))

-- $math
-- == Arithmetic Helper Functions
--
-- The following helper functions allow us to omit binds in arithmetic
-- expressions.

minus :: (SigFunctor sig) => Prog sig Int -> Prog sig Int -> Prog sig Int
minus = liftM2 (-)

mult :: (SigFunctor sig) => Prog sig Int -> Prog sig Int -> Prog sig Int
mult = liftM2 (*)

add :: (SigFunctor sig) => Prog sig Int -> Prog sig Int -> Prog sig Int
add = liftM2 (+)

lt :: Ord a => (SigFunctor sig) => Prog sig a -> Prog sig a -> Prog sig Bool
lt = liftM2 (<)

le :: Ord a => (SigFunctor sig) => Prog sig a -> Prog sig a -> Prog sig Bool
le = liftM2 (<=)

eq :: Eq a => (SigFunctor sig) => Prog sig a -> Prog sig a -> Prog sig Bool
eq = liftM2 (==)

neq :: Eq a => (SigFunctor sig) => Prog sig a -> Prog sig a -> Prog sig Bool
neq = liftM2 (/=)

ge :: Ord a => (SigFunctor sig) => Prog sig a -> Prog sig a -> Prog sig Bool
ge = liftM2 (>=)

gt :: Ord a => (SigFunctor sig) => Prog sig a -> Prog sig a -> Prog sig Bool
gt = liftM2 (>)

-------------
-- File IO --
-------------

-- $poem
--
-- The following example computations for reading and writing a poem to a file
-- were taken from <https://learnxinyminutes.com/docs/standard-ml/>.

writePoem
  :: (Let ML :<: sig, OutputFile :<: sig) => Prog sig FilePath -> Prog sig ()
writePoem fileName = do
  file <- let_ @ML (TextIO.openOut fileName)
  _ <- let_ @ML (TextIO.output file (return "Roses are red,\nViolets are blue.\n"))
  _ <- let_ @ML (TextIO.output file (return "I have a gun.\nGet in the van.\n"))
  TextIO.closeOut file

readPoem
  :: (Let ML :<: sig, InputFile :<: sig)
  => Prog sig FilePath -> Prog sig [String]
readPoem fileName = do
  file <- let_ @ML (TextIO.openIn fileName)
  poem <- let_ @ML (TextIO.inputAll file)
  _    <- let_ @ML (TextIO.closeIn file)
  lines <$> poem

----------------
-- References --
----------------

decrementToZero
  :: forall ref sig
   . (Typeable (ref Int), Ref ref :<: sig, Let ML :<: sig)
  => Prog sig (ref Int) -> Prog sig ()
decrementToZero pRef = do
  pRef' <- let_ @ML pRef
  ref <- pRef'
  ifM (readRef ref `lt` return 0)
    {- then -} (writeRef ref 0)
    {- else -} (whileM_ (readRef ref `gt` return 0)
                  {- do -} (writeRef ref =<< (readRef ref `minus` return 1)))

-----------
-- Tests --
-----------

testMLExamples :: Spec
testMLExamples = describe "Control.Prog.Example.ML" $ do
  testPoem
  testDecrementToZero

testPoem :: Spec
testPoem = context "writePoem and readPoem" $ do
  after_ (removeFile "roses.txt") $ do
    it "performs all outputs in a call-by-value setting" $ do
      result <- runM (runOutputFile @IO (runInputFile @IO (runCBV @ML (writePoem (return "roses.txt") >> readPoem (return "roses.txt")))))
      result `shouldBe` ["Roses are red,", "Violets are blue.", "I have a gun.", "Get in the van."]

testDecrementToZero :: Spec
testDecrementToZero = context "decrementToZero" $ do
  it "decrements references to negative numbers to zero" $ do
    result <- runM $ runIORef $ runCBV @ML $ do
      ref <- newRef @IORef (-42)
      decrementToZero (return ref)
      readRef ref
    result `shouldBe` 0
  it "decrements references to zero to zero" $ do
    result <- runM $ runIORef $ runCBV @ML $ do
      ref <- newRef @IORef 0
      decrementToZero (return ref)
      readRef ref
    result `shouldBe` 0
  it "decrements references to positive numbers to zero" $ do
    result <- runM $ runIORef $ runCBV @ML $ do
      ref <- newRef @IORef 42
      decrementToZero (return ref)
      readRef ref
    result `shouldBe` 0
