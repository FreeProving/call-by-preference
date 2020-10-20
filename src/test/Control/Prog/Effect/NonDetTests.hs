{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.NonDetTests (testNonDetEffect) where

import           Prelude                    hiding (fail)

import           Control.Monad              (replicateM, replicateM_)
import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               ((:<:), Let, Prog, final, let_, run,
                                             runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.NonDet (NonDet, choose, fail, runNonDet)
import           Control.Prog.Effect.State  (State, get, modify, runState)
import           Control.Prog.Util.Tags     (Tag)

--------------------------
-- Example Computations --
--------------------------

data Toss = Heads | Tails
 deriving (Eq, Show)

toss :: (NonDet :<: sig) => Prog sig Toss
toss = choose (return Heads) (return Tails)

-- | An example computation that shares a stateful computation that increments
--   a counter across both branches of a non-deterministic computation.
--
--   If the counter is global with respect to the 'NonDet' effect (i.e.,
--   the 'runNonDet' handler runs before the 'runState' handler), it is
--   incremented once in a call-by-value setting and twice (i.e., once for
--   every branch) when call-by-name is used. The behavior is the same for
--   call-by-need as for call-by-name. Only if the result of incrementing the
--   counter was demanded before the computation branched or the 'final'
--   handler is invoked after the 'runNonDet' handler, it would be shared
--   across both branches of the computation.
--
--   If the counter is local with respect to the 'NonDet' effect (i.e.,
--   the 'runNonDet' handler runs after the 'runState' handler), information
--   can still flow from one branch to another if the 'final' handler is invoked
--   after the 'runNonDet' handler in a call-by-need setting.
letIncChooseComp
  :: forall tag sig
   . (State Int :<: sig, NonDet :<: sig, Let tag :<: sig)
  => Prog sig Int
letIncChooseComp = do
  inc <- let_ @tag (modify (succ @Int) >> get)
  choose inc inc

-- | An example computation that chooses between two shared a 'toss'es.
--
--   This example demonstrates, that the 'final' handler should be invoked
--   before the handler for the 'NonDet' effect in a call-by-need setting such
--   that shared values are not memorized across branches of non-deterministic
--   computations. Otherwise, the 'toss' in the first branch returns both
--   'Heads' and 'Tails' but only memorizes the last value (i.e., 'Tails').
--   Thus, the 'toss' in the second branch only returns 'Tails'.
--
--   In a call-by-name and call-by-value setting as well as when the 'final']
--   handler is invoked after the 'NonDet' handler in a call-by-need setting,
--   this example behaves as expected.
letChooseComp
  :: forall tag sig
   . (NonDet :<: sig, Let tag :<: sig)
  => Prog sig Toss
letChooseComp = do
  mx <- let_ @tag toss
  choose mx mx

-----------
-- Tests --
-----------

testNonDetEffect :: Spec
testNonDetEffect = describe "Control.Prog.Effect.NonDet" $ do
  testRunNonDet
  testCBV
  testCBN
  testCBNeed

testRunNonDet :: Spec
testRunNonDet = context "runNonDet" $ do
  it "finds all results" $ do
    let result = run (runNonDet (replicateM 2 toss))
    result `shouldBe` [[Heads, Heads], [Heads, Tails], [Tails, Heads], [Tails, Tails]]
  it "finds no results if computation fails" $ do
    let result = run (runNonDet (replicateM_ 2 toss >> fail @()))
    result `shouldBe` []
  it "does not discard results from branch after failure" $ do
    let result = run (runNonDet (choose fail toss))
    result `shouldBe` [Heads, Tails]
  it "does not discard results from branch before failure" $ do
    let result = run (runNonDet (choose toss fail))
    result `shouldBe` [Heads, Tails]

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

-- | Tests the interaction of the 'NonDet' and 'Let' effects in a call-by-value
--   setting.
testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "increments the global counter once before the computation branches out" $ do
    let result = run (runState @Int 0 (runNonDet (runCBV @Tag (letIncChooseComp @Tag))))
    result `shouldBe` (1, [1, 1])
  it "chooses the value of a shared variable before the computation branches out" $ do
    let result = run (runNonDet (runCBV @Tag (letChooseComp @Tag)))
    result `shouldBe` [Heads, Heads, Tails, Tails]

-- | Tests the interaction of the 'NonDet' and 'Let' effects in a call-by-name
--   setting.
testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "increments the global counter once in every branch of the computation" $ do
    let result = run (runState @Int 0 (runNonDet (runCBN @Tag (letIncChooseComp @Tag))))
    result `shouldBe` (2, [1, 2])
  it "chooses the value of a shared variable in every branch of the computation" $ do
    let result = run (runNonDet (runCBN @Tag (letChooseComp @Tag)))
    result `shouldBe` [Heads, Tails, Heads, Tails]

-- | Tests the interaction of the 'NonDet' and 'Let' effects in a call-by-need
--   setting.
testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "does not share the result of incrementing a gloabl counter across branches\
     \ if the `final` handler is invoked before the handler for `NonDet`" $ do
    let result = run (runState @Int 0 (runNonDet (final (runCBNeed @Tag (letIncChooseComp @Tag)))))
    result `shouldBe` (2, [1, 2])
  it "shares the result of incrementing a gloabl counter across branches\
     \ if the `final` handler is invoked after the handler for `NonDet`" $ do
    let result = run (runState @Int 0 (final (runNonDet (runCBNeed @Tag (letIncChooseComp @Tag)))))
    result `shouldBe` (1, [1, 1])
  it "does not share the result of incrementing a local counter across branches\
     \ if the `final` handler is invoked before the handler for `NonDet`" $ do
    let result = run (runNonDet (runState @Int 0 (final (runCBNeed @Tag (letIncChooseComp @Tag)))))
    result `shouldBe` [(1, 1), (1, 1)]
  it "shares the result of incrementing a local counter across branches\
     \ if the `final` handler is invoked after the handler for `NonDet`" $ do
    let result = run (final (runNonDet (runState @Int 0 (runCBNeed @Tag (letIncChooseComp @Tag)))))
    result `shouldBe` [(1, 1), (0, 1)]
  it "replays all choices for the value of a shared variable\
     \ if the `final` handler is invoked before the handler for `NonDet`" $ do
    let result = run (runNonDet (final (runCBNeed @Tag (letChooseComp @Tag))))
    result `shouldBe` [Heads, Tails, Heads, Tails]
  it "only remembers the last choice for the value of a shared variable\
     \ if the `final` handler is invoked after the handler for `NonDet`" $ do
    let result = run (final (runNonDet (runCBNeed @Tag (letChooseComp @Tag))))
    result `shouldBe` [Heads, Tails, Tails]
