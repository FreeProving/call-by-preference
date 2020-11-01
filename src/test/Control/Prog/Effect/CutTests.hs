{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.CutTests (testCutEffect) where

import           Prelude                    hiding (fail)

import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               ((:<:), Let, Prog, final, let_, run,
                                             runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Cut    (Cut, call, cut, cutfail, runCut)
import           Control.Prog.Effect.NonDet (NonDet, choose, fail, runNonDet)
import           Control.Prog.Effect.Reader (ask, local, runReader)
import           Control.Prog.Effect.Trace  (runTrace, trace)
import           Control.Prog.Util.Tags     (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that binds the 'cut' operator using 'let_' and uses
--   the bound computation in two branches of a non-deterministic computation
--   that each delimit the scope of the 'cut' using 'call'.
--
--   In a call-by-value setting, the 'cut' is evaluated before the choice and
--   thus does not truncate the search space at all. In a call-by-name setting,
--   the search space is truncated in both branches. In a call-by-need setting
--   the result depends on the order of the 'final' and 'runCut' handlers.
--   If the 'final' handler is invoked first, the result is the same as in the
--   call-by-name setting, since the result of the bound computation is not
--   memorized across the branches of the computation. On the other hand, if
--   the 'Cut' effect is handled first, the 'cut' in the second branch does not
--   truncate the search space because the result from the 'cut' in the first
--   branch is memorized.
letCutComp :: forall tag sig. (NonDet :<: sig, Cut :<: sig, Let tag :<: sig) => Prog sig Int
letCutComp = do
  cut' <- let_ @tag cut
  choose (call (choose (cut' >> return 1) (return 2)))
         (call (choose (cut' >> return 3) (return 4)))

-- | An example computation similar to 'letCutComp' that binds 'cutfail'
--   instead of 'cut'.
--
--   In a call-by-name setting, this computation behaves the same as
--   'letCutComp'. In a call-by-need setting this computation also
--   behaves like in a call-by-name setting. This is because the 'cutfail'
--   operation does not produce a result which could be memorized.
--   In consequence, the bound computation is repeated regardless of the order
--   of the 'runCut' and 'final' handlers.
--   In a call-by-value setting, this computation produces no result because
--   the computation fail immediately when the 'cutfail' is bound.
letCutfailComp :: forall tag sig. (NonDet :<: sig, Cut :<: sig, Let tag :<: sig) => Prog sig Int
letCutfailComp = do
  cutfail' <- let_ @tag cutfail
  choose (call (choose (choose (return 1) cutfail') (return 2)))
         (call (choose (choose (return 3) cutfail') (return 4)))

-- | An example computation that demonstrates that adding the 'runCut' handler
--   to a program that does not use the 'Cut' effect, changes the semantics
--   already.
letChooseComp :: forall tag sig. (NonDet :<: sig, Let tag :<: sig) => Prog sig Int
letChooseComp = do
  mx <- let_ @tag (choose (return 1) (return 2))
  choose mx mx

-----------
-- Tests --
-----------

testCutEffect :: Spec
testCutEffect = describe "Control.Prog.Effect.Cut" $ do
  testRunCut
  testCBV
  testCBN
  testCBNeed

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
  it "does not duplicate branches after call" $ do
    let result = run (runNonDet (runCut (choose (call (choose (return 1) (return 2))) (return 3))))
    result `shouldBe` [1, 2, 3 :: Int]

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
    it "finds all results of choices after the cut" $ do
      let result = run (runTrace (runNonDet (runCut (cut >> choose (trace "True" (return True)) (trace "False" (return False))))))
      result `shouldBe` (["True", "False"], [True, False])
    it "finds all results in the presence of failures" $ do
      let result = run (runTrace (runNonDet (runCut (choose (trace "True" (choose fail (return True))) (trace "False" (return False))))))
      result `shouldBe` (["True", "False"], [True, False])
    it "does not limit the scope of cut if there is no enclosing call" $ do
      let result = run (runTrace (runNonDet (runCut (choose (choose (cut >> trace "1" (return 1)) (trace "2" (return 2))) (trace "2" (return 2))))))
      result `shouldBe` (["1"], [1 :: Int])
    it "limits the scope of cut if there is a enclosing call" $ do
      let result = run (runTrace (runNonDet (runCut (choose (call (choose (cut >> trace "1" (return 1)) (trace "2" (return 2)))) (trace "3" (return 3))))))
      result `shouldBe` (["1", "3"], [1, 3 :: Int])

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "does not truncate search space after 'cut' has been bound" $ do
    let result = run (runNonDet (runCut (runCBV @Tag (letCutComp @Tag))))
    result `shouldBe` [1, 2, 3, 4]
  it "does not truncate search space after 'cutfail' has been bound" $ do
    let result = run (runNonDet (runCut (runCBV @Tag (letCutfailComp @Tag))))
    result `shouldBe` []

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "truncates search space whenever the bound 'cut' is used" $ do
    let result = run (runNonDet (runCut (runCBN @Tag (letCutComp @Tag))))
    result `shouldBe` [1, 3]
  it "truncates search space whenever the bound 'cutfail' is used" $ do
    let result = run (runNonDet (runCut (runCBN @Tag (letCutfailComp @Tag))))
    result `shouldBe` [1, 3]

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "does not memorize the result of 'cut' across branches\
     \ if the `final` handler is invoked before the handler for `Cut`" $ do
    let result = run (runNonDet (runCut (final (runCBNeed @Tag (letCutComp @Tag)))))
    result `shouldBe` [1, 3]
  it "memorizes the result of 'cut' across branches\
     \ if the `final` handler is invoked after the handler for `Cut`" $ do
    let result = run (runNonDet (final (runCut (runCBNeed @Tag (letCutComp @Tag)))))
    result `shouldBe` [1, 3, 4]
  it "memorizes the result of 'cut' across branches\
     \ if the `final` handler is invoked after the handler for `NonDet`" $ do
    let result = run (final (runNonDet (runCut (runCBNeed @Tag (letCutComp @Tag)))))
    result `shouldBe` [1, 3, 4]

  it "does not memorize the result of 'cutfail' across branches\
     \ if the `final` handler is invoked before the handler for `Cut`" $ do
    let result = run (runNonDet (runCut (final (runCBNeed @Tag (letCutfailComp @Tag)))))
    result `shouldBe` [1, 3]
  it "does not memorize the result of 'cutfail' across branches\
     \ if the `final` handler is invoked after the handler for `Cut`" $ do
    let result = run (runNonDet (final (runCut (runCBNeed @Tag (letCutfailComp @Tag)))))
    result `shouldBe` [1, 3]
  it "does not memorize the result of 'cutfail' across branches\
     \ if the `final` handler is invoked after the handler for `NonDet`" $ do
    let result = run (final (runNonDet (runCut (runCBNeed @Tag (letCutfailComp @Tag)))))
    result `shouldBe` [1, 3]

  it "replays all choices for the value of a shared variable\
     \if there is a handler for the 'Cut' effect after 'final'" $ do
    let result = run (runNonDet (final (runCBNeed @Tag (letChooseComp @Tag))))
    result `shouldBe` [1, 2, 1, 2]
  it "only memorizes the last choice for the value of a shared variable\
     \if there is a handler for the 'Cut' effect after 'final'" $ do
    let result = run (runNonDet (final (runCut (runCBNeed @Tag (letChooseComp @Tag)))))
    result `shouldBe` [1, 2, 2]
