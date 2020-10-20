{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.ProbTests (testProbEffect) where

import           Test.Hspec               (Spec, context, describe, it,
                                           shouldBe)

import           Control.Prog             ((:<:), Let, Prog, final, let_, run,
                                           runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Prob (Prob, choice, fromDist, normalize,
                                           runProb)
import           Control.Prog.Util.Tags   (Tag)

--------------------------
-- Example Computations --
--------------------------

data Toss = Heads | Tails
 deriving (Eq, Show)

fairToss :: (Prob :<: sig) => Prog sig Toss
fairToss = choice 0.5 (return Heads) (return Tails)

-- | An example computation that binds a 'fairToss' with 'let_' and tests
--   whether the result of both tosses is the same.
--
--   In a call-by-value and call-by-need setting, this computation always
--   returns @True@ because the computation that throws the coin is evaluated
--   only once. In a call-by-name setting on the other hand, the two occurences
--   of @toss@ may evaluate to two different values and thus @False@ is returned
--   in half of all cases.
eqTossComp :: forall tag sig. (Prob :<: sig, Let tag :<: sig) => Prog sig Bool
eqTossComp = do
  toss <- let_ @tag fairToss
  (==) <$> toss <*> toss

-----------
-- Tests --
-----------

testProbEffect :: Spec
testProbEffect = describe "Control.Prog.Effect.Prob" $ do
  testRunProb
  testCBV
  testCBN
  testCBNeed

testRunProb :: Spec
testRunProb = context "runProb" $ do
  it "returns both values probabilistically" $ do
    let result = run (runProb (choice 0.5 (return True) (return False)))
    normalize (fromDist result) `shouldBe` [(True, 0.5), (False, 0.5)]
  it "returns the first value with the specified probability" $ do
    let result = run (runProb (choice 0.25 (return True) (return False)))
    normalize (fromDist result) `shouldBe` [(True, 0.25), (False, 0.75)]
  it "scales the probability of nested computations" $ do
    let result = run (runProb (choice 0.5 (return True) (choice 0.5 (return True) (return False))))
    normalize (fromDist result) `shouldBe` [(True, 0.75), (False, 0.25)]
  it "also returns values with a probability of zero" $ do
    let result = run (runProb (choice 0.0 (return True) (return False)))
    normalize (fromDist result) `shouldBe` [(True, 0.0), (False, 1.0)]

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "evaluates bound computations only once" $ do
    let result = run (runProb (runCBV @Tag (eqTossComp @Tag)))
    normalize (fromDist result) `shouldBe` [(True, 1.0)]

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "evaluates bound computations every time they are used" $ do
    let result = run (runProb (runCBN @Tag (eqTossComp @Tag)))
    normalize (fromDist result) `shouldBe` [(True, 0.5), (False, 0.5)]

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "evaluates the shared computation at most once" $ do
    let result = run (runProb (final (runCBNeed @Tag (eqTossComp @Tag))))
    normalize (fromDist result) `shouldBe` [(True, 1.0)]
