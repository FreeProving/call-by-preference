{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.ReaderTests (testReaderEffect) where

import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               ((:<:), Let, Prog, final, let_, run,
                                             runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Reader (Reader, ask, local, runReader)
import           Control.Prog.Util.Tags     (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that reads a value before and after inverting it
--   'local'ly. Returns the values that has been read before, within and
--   after the call to 'local'.
--
--   This example is used to test that the interpretation of 'local' by the
--   'runReader' handler restores the locally overwritten value correctly.
localComp :: (Reader Bool :<: sig) => Prog sig (Bool, Bool, Bool)
localComp = do
  x <- ask
  y <- local not ask
  z <- ask
  return (x, y, z)

-- | An example computation that reads a boolean value in a 'local' reader
--   where the value is flipped using an 'ask' computation that is bound by
--   a 'let_'.
--
--   In a call-by-value setting, the value is read upon the call to 'let_'.
--   Thus, the non-inverted value is read in this case. In a call-by-name and
--   call-by-need setting, the value is read within the scope of 'local' for
--   the first time. Thus, the inverted value is read in this case.
letAskLocalComp :: forall tag sig. (Reader Bool :<: sig, Let tag :<: sig) => Prog sig Bool
letAskLocalComp = do
  x <- let_ @tag ask
  local not x

-- | Like 'letAskLocalComp' but the value is demanded before the call to 'local'.
--
--   Returns the value that was computed before and within the call to 'local'.
--
--   Because the value is demanded outside the scope of 'local', the
--   non-inverted value is read in a call-by-need setting. In a call-by-name
--   setting, the two occurrences of the reading computation is not shared.
--   Thus, the inverted value is read outside of 'local' and the inverted
--   value is read within 'local'.
letAskLocalComp' :: forall tag sig. (Reader Bool :<: sig, Let tag :<: sig) => Prog sig (Bool, Bool)
letAskLocalComp' = do
  x <- let_ @tag ask
  y <- x
  z <- local not x
  return (y, z)

-----------
-- Tests --
-----------

testReaderEffect :: Spec
testReaderEffect = describe "Control.Prog.Effect.Reader" $ do
  testRunReader
  testCBV
  testCBN
  testCBNeed

testRunReader :: Spec
testRunReader = context "runReader" $ do
  it "restores previous value after local computation" $ do
    let result = run (runReader True localComp)
    result `shouldBe` (True, False, True)

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "reads non-`local` value if bound by `let_` outside" $ do
    let result = run (runReader True (runCBV @Tag (letAskLocalComp @Tag)))
    result `shouldBe` True
  it "reads the same value inside and outside `local`" $ do
    let result = run (runReader True (runCBV @Tag (letAskLocalComp' @Tag)))
    result `shouldBe` (True, True)

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "reads `local` value even if bound by `let_` outside" $ do
    let result = run (runReader True (runCBN @Tag (letAskLocalComp @Tag)))
    result `shouldBe` False
  it "reads different values inside and outside `local`" $ do
    let result = run (runReader True (runCBN @Tag (letAskLocalComp' @Tag)))
    result `shouldBe` (True, False)

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "reads `local` value even if bound by `let_` outside" $ do
    let result = run (runReader True (final (runCBNeed @Tag (letAskLocalComp @Tag))))
    result `shouldBe` False
  it "reads value from the scope where the value has been demanded first" $ do
    let result = run (runReader True (final (runCBNeed @Tag (letAskLocalComp' @Tag))))
    result `shouldBe` (True, True)
