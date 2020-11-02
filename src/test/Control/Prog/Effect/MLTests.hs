{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.MLTests where

import           Prelude                        hiding (fail, fst, repeat, snd,
                                                 take, undefined)

import           Control.Monad                  (liftM2)
import           System.Directory               (removeFile)
import           Test.Hspec                     (Spec, after_, context,
                                                 describe, it, shouldBe)

import           Control.Prog
import           Control.Prog.Effect.InputFile  (InputFile, runInputFile)
import           Control.Prog.Effect.OutputFile (OutputFile, runOutputFile)
import qualified Control.Prog.Effect.TextIO     as TextIO


-- ML-like language

data ML

{-
The arguments of all functions are bound via let in order to get cbv
-}
fac :: (Let ML :<: sig) => Prog sig Int -> Prog sig Int
fac pn = do
  pn' <- let_ @ML pn
  n <- pn'
  if n <= 0 then return 0
            else mult pn' (fac (minus pn' (return 1)))

{-
Rigid functions to not have to use let to bind arguments
-}
minus :: (SigFunctor sig) => Prog sig Int -> Prog sig Int -> Prog sig Int
minus = liftM2 (-)

mult :: (SigFunctor sig) => Prog sig Int -> Prog sig Int -> Prog sig Int
mult = liftM2 (*)

add :: (SigFunctor sig) => Prog sig Int -> Prog sig Int -> Prog sig Int
add = liftM2 (+)


writePoem :: (Let ML :<: sig, OutputFile :<: sig) => Prog sig String -> Prog sig ()
writePoem fileName = do
  file <- let_ @ML (TextIO.openOut fileName)
  _ <- let_ @ML (TextIO.output file (return "Roses are red,\nViolets are blue.\n"))
  _ <- let_ @ML (TextIO.output file (return "I have a gun.\nGet in the van.\n"))
  TextIO.closeOut file

readPoem :: (Let ML :<: sig, InputFile :<: sig) => Prog sig String -> Prog sig [String]
readPoem fileName = do
  file <- let_ @ML (TextIO.openIn fileName)
  poem <- let_ @ML (TextIO.inputAll file)
  _    <- let_ @ML (TextIO.closeIn file)
  lines <$> poem


strict :: (Let ML :<: sig) => Prog sig Int
strict = do
  _ <- let_ @ML (fac (return 10000))
  return 42

-- decrementToZero r ::
-- decrementToZero =


-----------
-- Tests --
-----------

testMLEffects :: Spec
testMLEffects = describe "Control.Prog.Effect.MLTests" $ do
  testOutput

testOutput :: Spec
testOutput = context "output effect" $ do
  after_ (removeFile "roses.txt") $ do
    it "performs all outputs in cbv" $ do
      result <- runM (runOutputFile @IO (runInputFile @IO (runCBV @ML (writePoem (return "roses.txt") >> readPoem (return "roses.txt")))))
      result `shouldBe` ["Roses are red,", "Violets are blue.", "I have a gun.", "Get in the van.\n"]
  it "it evaluates effect-free expressions" $ do
    let result = run (runCBV @ML strict)
    result `shouldBe` 42
