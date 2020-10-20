{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.StateTests (testStateEffect) where

import           Prelude                   hiding (fail)

import           Control.Monad             (join)
import           Test.Hspec                (Spec, context, describe, it,
                                            shouldBe)
import           Test.QuickCheck           (property, (===))

import           Control.Prog              ((:<:), Let, Prog, final, let_, run,
                                            runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Error (Error, runError, throw)
import           Control.Prog.Effect.State (State, get, modify, put, runState,
                                            runTransaction)
import           Control.Prog.Util.Tags    (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that should throw the exception @69@. This example
--   demonstrates that when transactions are used to handle the 'State' effect,
--   the modifications of the state by computations that throw an error are not
--   committed.
throwTransactionComp :: forall sig. (State Int :<: sig, Error Int :<: sig) => Prog sig ()
throwTransactionComp = do
  put @Int 23
  e <- fmap (3 *) get
  _ <- throw @Int e
  put @Int 34

-- | An example computation that binds 'get' with 'let_' and uses the bound
--   operation to read the state twice. Before the state is read, it is
--   incremented first. Returns the sum of the read states.
--
--    In a call-by-value setting, the initial state is read when the computation
--    is bound by 'let_'. Thus, the initial state is doubled. In a call-by-name
--    and call-by-need setting, the state is read after it is incremented.
--    Because the state is read only the first time when call-by-need is used,
--    the result is one less in this case.
letGetIncComp :: forall tag sig. (State Int :<: sig, Let tag :<: sig) => Prog sig Int
letGetIncComp = do
  get' <- let_ @tag get
  modify @Int succ
  x <- get'
  modify @Int succ
  y <- get'
  return (x + y)

-----------
-- Tests --
-----------

testStateEffect :: Spec
testStateEffect = describe "Control.Prog.Effect.State" $ do
  testRunState
  testRunTransaction
  testCBV
  testCBN
  testCBNeed

testRunState :: Spec
testRunState = context "runState" $ do
  it "should have no effect setting the state to the current state" $ property $ \(s0 :: Int) -> do
    run (runState s0 (get @Int >>= put)) === run (runState s0 (return ()))
  it "should get most recently put value" $ property $ \(s0 :: Int) (s' :: Int) -> do
    run (runState s0 (put s' >> get)) === run (runState s0 (put s' >> return s'))

testRunTransaction :: Spec
testRunTransaction = context "runTransaction" $ do
  it "should commit the final state of successful computations" $ do
    let result = run (runState @Int 0 (runError @Int (runTransaction @Int (put @Int 42))))
    result `shouldBe` (42, Right ())
  it "should discard the modifications to the state by failing computations" $ do
    let result = run (runState @Int 0 (runError @Int (runTransaction @Int throwTransactionComp)))
    result `shouldBe` (0, Left 69)
  it "should not discard the modifications when no transactions are used" $ do
    let result = run (runState @Int 0 (runError @Int throwTransactionComp))
    result `shouldBe` (23, Left 69)

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "reads the state when the computation is bound immediately" $ do
    let result = run (runState @Int 1 (runCBV @Tag (letGetIncComp @Tag)))
    result `shouldBe` (3, 2)

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "reads the modified state when the bound computation is used" $ do
    let result = run (runState @Int 1 (runCBN @Tag (letGetIncComp @Tag)))
    result `shouldBe` (3, 5)

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "cannot access the state of the `Let` handler accidentally" $ do
    let result = run (runState @Int 0 (final (runCBNeed @Tag (join (let_ @Tag (get @Int))))))
    result `shouldBe` (0, 0)
  it "reads and memorizes the modified state when the bound computation is\
     \ used for the first time" $ do
    let result = run (runState @Int 1 (final (runCBNeed @Tag (letGetIncComp @Tag))))
    result `shouldBe` (3, 4)
