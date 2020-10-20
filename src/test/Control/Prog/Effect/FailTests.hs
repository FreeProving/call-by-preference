{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.FailTests (testFailEffect) where

import           Prelude                  hiding (fail)

import           Control.Monad            (void)
import           Test.Hspec               (Spec, context, describe, it,
                                           shouldBe)

import           Control.Prog             ((:<:), Let, Prog, final, let_, run,
                                           runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Fail (Fail, fail, runFail)
import           Control.Prog.Util.Tags   (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that binds 'fail' with 'let_' and returns without
--   ever demanding the bound computation.
--
--   This example fails in a call-by-value setting but returns successfully in
--   both a call-by-name and call-by-value setting.
letFailComp :: forall tag sig. (Fail :<: sig, Let tag :<: sig) => Prog sig ()
letFailComp = void (let_ @tag (fail @()))

-----------
-- Tests --
-----------

testFailEffect :: Spec
testFailEffect = describe "Control.Prog.Effect.Fail" $ do
  testRunFail
  testCBV
  testCBN
  testCBNeed

testRunFail :: Spec
testRunFail = context "runFail" $ do
  it "returns `Just` the result if the computation does not fail" $ do
    let result = run (runFail (return ()))
    result `shouldBe` Just ()
  it "returns `Nothing` if the computation fails" $ do
    let result = run (runFail fail) :: Maybe ()
    result `shouldBe` Nothing

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "fails even if the computation is not demanded" $ do
    let result = run (runFail (runCBV @Tag (letFailComp @Tag)))
    result `shouldBe` Nothing

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "does not fail if the computation is not demanded" $ do
    let result = run (runFail (runCBN @Tag (letFailComp @Tag)))
    result `shouldBe` Just ()

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "does not fail if the computation is not demanded" $ do
    let result = run (runFail (final (runCBNeed @Tag (letFailComp @Tag))))
    result `shouldBe` Just ()
