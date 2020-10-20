{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.TraceTests where

import           Test.Hspec                (Spec, context, describe, it,
                                            shouldBe)

import           Control.Prog              ((:<:), Let, Prog, final, let_, run,
                                            runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Trace (Trace, runTrace, trace)
import           Control.Prog.Util.Tags    (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that binds a computation using 'let_', adds its
--   result to itself and logs debugging messages before every operation.
--
--   In every setting, the @let@ message is logged first.In a call-by-value
--   setting, the bound computation is evaluated immediately. Thus, @return@
--   is logged before @add@. In a call-by-name and call-by-need setting, on the
--   other hand, the @add@ message is logged first. The @return@ message is
--   logged twice in a call-by-need setting but only once when call-by-need
--   is used.
letTraceComp :: forall tag sig. (Trace :<: sig, Let tag :<: sig) => Prog sig Int
letTraceComp = do
  mx <- trace "let" (let_ @tag (trace "return" (return 21)))
  trace "add" ((+) <$> mx <*> mx)

-----------
-- Tests --
-----------

testTraceEffect :: Spec
testTraceEffect = describe "Control.Prog.Effect.Trace" $ do
  testRunTrace
  testCBV
  testCBN
  testCBNeed

testRunTrace :: Spec
testRunTrace = context "runTrace" $ do
  it "collects messages" $ do
    let comp = trace "test" (return ())
        (messages, ()) = run (runTrace comp)
    messages `shouldBe` ["test"]
  it "collects messages in right order" $ do
    let comp1 = trace "previous" (return ())
        comp2 = trace "outer" (trace "inner" (return ()))
        comp3 = trace "next" (return ())
        (messages, ()) = run (runTrace (comp1 >> comp2 >> comp3))
    messages `shouldBe` ["previous", "outer", "inner", "next"]

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "logs the messages of bound computations when they are bound" $ do
    let result = run (runTrace (runCBV @Tag (letTraceComp @Tag)))
    result `shouldBe` (["let", "return", "add"], 42)

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "logs the messages of bound computations whenever they are used" $ do
    let result = run (runTrace (runCBN @Tag (letTraceComp @Tag)))
    result `shouldBe` (["let", "add", "return", "return"], 42)

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "logs the messages of shared computations at most once" $ do
    let result = run (runTrace (final (runCBNeed @Tag (letTraceComp @Tag))))
    result `shouldBe` (["let", "add", "return"], 42)
