{-# LANGUAGE TypeApplications #-}

module Control.Prog.Effect.DeferredTests (testDeferredEffect) where

import           Test.Hspec                   (Spec, context, describe, it,
                                               shouldBe)

import           Control.Prog                 (run)
import           Control.Prog.Effect.Deferred (force, runDeferred)
import           Control.Prog.Effect.Reader   (local, runReader)
import           Control.Prog.Effect.State    (execState, modify, runState)

testDeferredEffect :: Spec
testDeferredEffect = describe "Control.Prog.Effect.Deferred" $ do
  testRunDeferred

testRunDeferred :: Spec
testRunDeferred = context "runDeferred" $ do
  it "does not run deferred computation when the value is not forced" $ do
    let result = run (execState @Int 0 (runDeferred (modify @Int succ) (return ())))
    result `shouldBe` 0
  it "runs the deferred computation when its value is forced" $ do
    let result = run (execState @Int 0 (runDeferred (modify @Int succ) (force @())))
    result `shouldBe` 1
  it "runs the deferred computation at most once" $ do
    let result = run (execState @Int 0 (runDeferred (modify @Int succ) (force @() >> force @())))
    result `shouldBe` 1
  it "runs the deferred computation at most once in the presence of scoped effects" $ do
    let result = run (runReader False (execState @Int 0 (runDeferred (modify @Int succ) (local not (force @()) >> force @()))))
    result `shouldBe` 1
