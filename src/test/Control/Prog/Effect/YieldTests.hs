{-# LANGUAGE TypeApplications    #-}

module Control.Prog.Effect.YieldTests (testYieldEffect) where

import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               (run)
import           Control.Prog.Effect.Writer (runWriter, tell)
import           Control.Prog.Effect.Yield  (runYield, yield)

testYieldEffect :: Spec
testYieldEffect = describe "Control.Prog.Effect.Yield" $ do
  testRunYield

testRunYield :: Spec
testRunYield = context "runYield" $ do
  it "does not run action when no value is yielded" $ do
    let result = run (runWriter @[Int] (runYield @Int (\x -> do { tell [x]; return True }) (return ())))
    result `shouldBe` ([], Just ())
  it "runs the action when a value is yielded" $ do
    let result = run (runWriter @[Int] (runYield @Int (\x -> do { tell [x]; return True }) (yield @Int 0)))
    result `shouldBe` ([0], Just ())
  it "runs the action again when another value is yielded" $ do
    let result = run (runWriter @[Int] (runYield @Int (\x -> do { tell [x]; return True }) (yield @Int 0 >> yield @Int 1)))
    result `shouldBe` ([0, 1], Just ())
  it "cancels the computations when 'False' is returned" $ do
    let result = run (runWriter @[Int] (runYield @Int (\x -> do { tell [x]; return False }) (yield @Int 0 >> yield @Int 1)))
    result `shouldBe` ([0], Nothing)
