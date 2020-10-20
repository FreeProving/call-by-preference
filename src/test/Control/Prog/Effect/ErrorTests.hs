{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.ErrorTests (testErrorEffect) where

import           Control.Monad.Extra       (ifM)
import           Test.Hspec                (Spec, context, describe, it,
                                            shouldBe)

import           Control.Prog              ((:<:), Let, Prog, final, let_, run,
                                            runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Error (Error, catch, runError, throw)
import           Control.Prog.Effect.State (State, evalState, get, modify)
import           Control.Prog.Util.Tags    (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that demonstrates the influence of the order of
--   the 'runError' and 'runState' handlers.
--
--   If the 'Error' effect is handled first, modifications to the state by
--   computations that throw an error are preserved after the error has been
--   caught. If the 'State' effect is handled first, modifications to the
--   state are discarded when the computation throws an error.
errorStateComp :: (State Int :<: sig, Error () :<: sig) => Prog sig Int
errorStateComp = do
  modify @Int succ
  catch @() (modify @Int succ) return
  catch @() (modify @Int succ >> throw ()) return
  get

-- | An example computation that binds a stateful computation with 'let_' that
--   throws an error if the state is @False@. The bound computation is run
--   three times and thrown errors are caught. During the second run, the
--   state is inverted.
--
--   Returns the result (i.e., @"Success"@) or thrown error (i.e., @"Error"@)
--   of the three attempts to run the bound computation.
--
--   If the state is initially @False@, this example behaves as follows.
--
--    * In a call-by-value setting, the error that occurs while evaluating the
--      computation that is passed to 'let_' is thrown directly.
--
--    * In a call-by-name setting, the first and last run throw an exception
--      and the second one does not.
--
--    * The call-by-need setting is more interesting. The first run throws an
--      error. Thus, there is no value the 'runCBNeed' handler could memorize
--      and the computation is executed again in the second run. Since the state
--      is inverted this time, the computation succeeds and a value is stored by
--      the handler. In consequence, the computation is not repeated in the last
--      run and the same result as in the second one is returned.
letErrorStateComp
  :: forall tag sig
   . (State Bool :<: sig, Error String :<: sig, Let tag :<: sig)
  => Prog sig (String, String, String)
letErrorStateComp = do
  mx <- let_ @tag $ ifM get (return "Success") (throw "Error")
  x <- catch mx return
  modify not
  y <- catch mx return
  modify not
  z <- catch mx return
  return (x, y, z)

-- | An example computation that demonstrates the influence of the order of
--   the 'runError' and 'final' handlers in a call-by-need setting.
--
--   The computation demands the evaluation of a shared variable in a 'catch'
--   and throws an error afterwards. The 'State' effect maintains a counter
--   that is used for the shared computation to keep track how often it is
--   evaluated.
--
--   If the 'final' handler is applied first, the result computed for the
--   shared computation in the error handler is memorized (i.e., the state
--   is incremented once).
--   Otherwise, the computed value is discarded and the computation is
--   evaluated after the 'catch' again such that the counter is incremented
--   a second time.
demandAndThrowComp
  :: forall tag sig
   . (State Int :<: sig, Error () :<: sig, Let tag :<: sig)
  => Prog sig Int
demandAndThrowComp = do
  mx <- let_ @tag (modify @Int succ >> get)
  catch @() (mx >>= const (throw ())) return
  mx

-----------
-- Tests --
-----------

testErrorEffect :: Spec
testErrorEffect = describe "Control.Prog.Effect.Error" $ do
  testRunError
  testCBV
  testCBN
  testCBNeed

testRunError :: Spec
testRunError = context "runError" $ do
  it "should return value of computations that do not throw errors" $ do
    let result = run (runError (return ())) :: Either String ()
    result `shouldBe` Right ()
  it "should return uncaught errors" $ do
    let result = run (runError (throw "Error")) :: Either String ()
    result `shouldBe` Left "Error"
  it "should catch errors" $ do
    let result = run (runError @String @String (catch (throw "Error") (return . ("Caught " ++))))
    result `shouldBe` Right "Caught Error"
  it "should throw errors thrown by error handler" $ do
    let result = run (runError @String @() (catch (throw "Error") (throw . ("Other " ++))))
    result `shouldBe` Left "Other Error"
  it "preserves state manipulations of failed computation when the `Error`\
     \ effect is handled first." $ do
    let result = run (evalState @Int 0 (runError @() errorStateComp))
    result `shouldBe` Right 3
  it "discards state manipulations of failed computation when the `Error`\
     \ effect is handled first." $ do
    let result = run (runError @() (evalState @Int 0 errorStateComp))
    result `shouldBe` Right 2

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "runs the bound computation only once when it is defined" $ do
    let result = run (evalState False (runError @String (runCBV @Tag (letErrorStateComp @Tag))))
    result `shouldBe` Left "Error"

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "runs the bound computation every time it is used" $ do
    let result = run (evalState False (runError @String (runCBN @Tag (letErrorStateComp @Tag))))
    result `shouldBe` Right ("Error", "Success", "Error")

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "reruns the computation only if it fails" $ do
    let result = run (evalState False (runError @String (final (runCBNeed @Tag (letErrorStateComp @Tag)))))
    result `shouldBe` Right ("Error", "Success", "Success")
  it "discards values of shared variables evaluated by failed computations\
     \ if the `final` handler is invoked before the handler for `NonDet`" $ do
    let result = run (evalState @Int 0 (runError @() (final (runCBNeed @Tag (demandAndThrowComp @Tag)))))
    result `shouldBe` Right 2
  it "preserves values of shared variables evaluated by failed computations\
     \ if the `final` handler is invoked after the handler for `NonDet`" $ do
    let result = run (evalState @Int 0 (final (runError @() (runCBNeed @Tag (demandAndThrowComp @Tag)))))
    result `shouldBe` Right 1
