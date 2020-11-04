{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.RefTests (testRefEffect) where

import           Control.Monad.ST           (RealWorld, stToIO)
import           Data.IORef                 (IORef)
import           Data.STRef                 (STRef)
import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               ((:<:), IDRef, Let, Prog,
                                             finalWithRef, let_, run,
                                             runCBNeedWithRef, runM)
import           Control.Prog.Effect.NonDet (NonDet, choose, runNonDet)
import           Control.Prog.Effect.Ref    (Ref, newRef, readRef, runIORef,
                                             runSTRef, writeRef)
import           Control.Prog.Util.Tags     (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that demonstrates that the 'runCBNeedWithRef'
--   handler produces incorrect results when 'IORef's or 'STRef's are used
--   instead of 'IDRef's.
--
--   The results are incorrect because the 'runIORef' and 'runSTRef'
--   handlers do not reset the state of the references after one branch
--   of a non-deterministic has been processed. Thus, the choice for a
--   shared variable is remembered across the branches of a
--   non-deterministic computation. The same problem can be reproduced with
--   'IDRef's as well as the 'Control.Prog.runCBNeed' handler by invoking the
--   the 'finalWithRef' or 'Control.Prog.final' handler before the handler of
--   the 'NonDet' effect.
--
--   The test suite contains more examples that demonstrate the importance
--   of the position of the 'Control.Prog.final' handler. The problems that
--   occur in these cases also apply to the 'runCBNeedWithRef' handler.
letChooseComp
 :: forall tag sig
  . (NonDet :<: sig, Let tag :<: sig)
 => Prog sig Bool
letChooseComp = do
  mx <- let_ @tag (choose (return True) (return False))
  choose mx mx

-----------
-- Tests --
-----------

testRefEffect :: Spec
testRefEffect = describe "Control.Prog.Effect.Ref" $ do
  testRunIORef
  testRunSTRef
  testRunCBNeedWithRef

testRefHandler
  :: forall loc sig
   . (Ref loc :<: sig)
  => (forall a. Prog sig a -> IO a)
  -> Spec
testRefHandler handler = do
  it "should read initial value" $ do
    result <- handler $ do
      ref <- newRef @loc @Int 42
      readRef ref
    result `shouldBe` 42
  it "should read most recently written value" $ do
    result <- handler $ do
      ref <- newRef @loc @Int 0
      writeRef ref 42
      readRef ref
    result `shouldBe` 42
  it "should allow values to be overwritten" $ do
    result <- handler $ do
      ref <- newRef @loc @Int 0
      writeRef ref 1
      writeRef ref 2
      readRef ref
    result `shouldBe` 2
  it "should create distinct references" $ do
    result <- handler $ do
      r1 <- newRef @loc @Int 1
      r2 <- newRef @loc @Int 2
      writeRef r1 3
      writeRef r2 4
      (,) <$> readRef r1 <*> readRef r2
    result `shouldBe` (3, 4)

testRunIORef :: Spec
testRunIORef = context "runIORef" $
  testRefHandler @IORef (runM . runIORef)

testRunSTRef :: Spec
testRunSTRef = context "runSTRef" $
  testRefHandler @(STRef RealWorld) (stToIO . runM . runSTRef @RealWorld)

--------------------------------------------
-- Tests 'Ref'-Based Call-By-Need Handler --
--------------------------------------------

testRunCBNeedWithRef :: Spec
testRunCBNeedWithRef = context "runCBNeedWithRef" $ do
  it "sharing non-deterministic choices behaves correctly with 'IDRef's" $ do
    let result = run (runNonDet (finalWithRef (runCBNeedWithRef @IDRef @Tag (letChooseComp @Tag))))
    result `shouldBe` [True, False, True, False]
  it "sharing non-deterministic choices can behave incorrectly correctly with 'IDRef's" $ do
    let result = run (finalWithRef (runNonDet (runCBNeedWithRef @IDRef @Tag (letChooseComp @Tag))))
    result `shouldBe` [True, False, False]
  it "sharing non-deterministic choices behaves incorrectly with 'IORef's" $ do
    result <- runM (runIORef (runNonDet (runCBNeedWithRef @IORef @Tag (letChooseComp @Tag))))
    result `shouldBe` [True, False, False]
  it "cannot be fixed by moving the 'runIORef' handler" $ do
    result <- runM (runNonDet (runIORef (runCBNeedWithRef @IORef @Tag (letChooseComp @Tag))))
    result `shouldBe` [True, False, False]
  it "sharing non-deterministic choices behaves incorrectly with 'STRef's" $ do
    result <- stToIO (runM (runSTRef @RealWorld (runNonDet (runCBNeedWithRef @(STRef RealWorld) @Tag (letChooseComp @Tag)))))
    result `shouldBe` [True, False, False]
  it "cannot be fixed by moving the 'runSTRef' handler" $ do
    result <- stToIO (runM (runNonDet (runSTRef @RealWorld (runCBNeedWithRef @(STRef RealWorld) @Tag (letChooseComp @Tag)))))
    result `shouldBe` [True, False, False]
