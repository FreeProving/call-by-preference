{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Control.Prog.Effect.CutTests where

import           Prelude                    hiding (fail)

import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               ((:<:), Prog, run)
import           Control.Prog.Effect.Cut    (Cut, call, cut, runCut)
import           Control.Prog.Effect.NonDet (NonDet, choose, fail, runNonDet)
import           Control.Prog.Effect.Reader (ask, local, runReader)
import           Control.Prog.Effect.Trace  (runTrace, trace)

-----------
-- Tests --
-----------

testCutEffect :: Spec
testCutEffect = describe "Control.Prog.Effect.Cut" $ do
  testRunCut

testRunCut :: Spec
testRunCut = context "runCut" $ do
  it "finds all results if there is no cut" $ do
    let result = run (runNonDet (runCut (choose (return True) (return False))))
    result `shouldBe` [True, False]
  it "finds only the first result if there is a cut" $ do
    let result = run (runNonDet (runCut (choose (cut >> return True) (return False))))
    result `shouldBe` [True]
  it "finds all results in the presence of failures" $ do
    let result = run (runNonDet (runCut (choose (choose fail (return True)) (return False))))
    result `shouldBe` [True, False]
  it "does not limit the scope of cut if there is no enclosing call" $ do
    let result = run (runNonDet (runCut (choose (choose (cut >> return 1) (return 2)) (return 3))))
    result `shouldBe` [1 :: Int]
  it "limits the scope of cut if there is a enclosing call" $ do
    let result = run (runNonDet (runCut (choose (call (choose (cut >> return 1) (return 2))) (return 3))))
    result `shouldBe` [1, 3 :: Int]

  context "with runReader" $ do
    it "finds all results if there is no cut" $ do
      let result = run (runReader False (runNonDet (runCut (choose (local not ask) ask))))
      result `shouldBe` [True, False]
    it "finds only the first result if there is a cut" $ do
      let result = run (runReader False (runNonDet (runCut (choose (local not (cut >> ask)) ask))))
      result `shouldBe` [True]
    it "finds all results in the presence of failures" $ do
      let result = run (runReader False (runNonDet (runCut (choose (local not (choose fail ask)) ask))))
      result `shouldBe` [True, False]
    it "does not limit the scope of cut if there is no enclosing call" $ do
      let result = run (runReader @Int 3 (runNonDet (runCut (choose (local @Int pred (choose (local @Int pred (cut >> ask)) ask)) ask))))
      result `shouldBe` [1 :: Int]
    it "limits the scope of cut if there is a enclosing call" $ do
      let result = run (runReader @Int 3 (runNonDet (runCut (choose (call (local @Int pred (choose (local @Int pred (cut >> ask)) ask))) ask))))
      result `shouldBe` [1, 3 :: Int]

  context "with runTrace" $ do
    it "finds all results if there is no cut" $ do
      let result = run (runTrace (runNonDet (runCut (choose (trace "True" (return True)) (trace "False" (return False))))))
      result `shouldBe` (["True", "False"], [True, False])
    it "finds only the first result if there is a cut" $ do
      let result = run (runTrace (runNonDet (runCut (choose (trace "True" (cut >> return True)) (trace "False" (return False))))))
      result `shouldBe` (["True"], [True])
    it "finds all results in the presence of failures" $ do
      let result = run (runTrace (runNonDet (runCut (choose (trace "True" (choose fail (return True))) (trace "False" (return False))))))
      result `shouldBe` (["True", "False"], [True, False])
    it "does not limit the scope of cut if there is no enclosing call" $ do
      let result = run (runTrace (runNonDet (runCut (choose (choose (cut >> trace "1" (return 1)) (trace "2" (return 2))) (trace "2" (return 2))))))
      result `shouldBe` (["1"], [1 :: Int])
    it "limits the scope of cut if there is a enclosing call" $ do
      let result = run (runTrace (runNonDet (runCut (choose (call (choose (cut >> trace "1" (return 1)) (trace "2" (return 2)))) (trace "3" (return 3))))))
      result `shouldBe` (["1", "3"], [1, 3 :: Int])
