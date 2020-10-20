{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Effect.WriterTests (testWriterEffect) where

import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               ((:<:), Let, Prog, final, let_, run,
                                             runCBN, runCBNeed, runCBV)
import           Control.Prog.Effect.Writer (Writer, execWriter, tell)
import           Control.Prog.Util.Tags     (Tag)

--------------------------
-- Example Computations --
--------------------------

-- | An example computation that uses 'let_' to bind three computations that
--   output the  letters @x@, @y@ and @z@ and demands the computations first
--   in the opposite order and then in the right order.
--
--   In a call-by-value setting, the computations are executed when they are
--   bound by the 'let_'. Thus, the output is collected in the order of the
--   bindings (i.e., @xyz@). In a call-by-need setting on the other hand, the
--   computations are executed only the first time they are used. Since the
--   computations are demanded in opposite order first, the output is collected
--   in reverse order (i.e., @zyx@). The second time the computations are
--   demanded, the output is not collected. In a call-by-name setting also the
--   computations are executed also the second time. Thus, the output is
--   collected backwards and forwards.
letTellComp :: forall tag sig. (Writer String :<: sig, Let tag :<: sig) => Prog sig ()
letTellComp = do
  mx <- let_ @tag (tell "x")
  my <- let_ @tag (tell "y")
  mz <- let_ @tag (tell "z")
  mz >> my >> mx >> mx >> my >> mz

-----------
-- Tests --
-----------

testWriterEffect :: Spec
testWriterEffect = describe "Control.Prog.Effect.Writer" $ do
  testRunWriter
  testCBV
  testCBN
  testCBNeed

testRunWriter :: Spec
testRunWriter = context "runWriter" $ do
  it "collects output" $ do
    let result = run (execWriter (tell "test"))
    result `shouldBe` "test"
  it "collects output in right order" $ do
    let result = run (execWriter (tell "a" >> tell "b" >> tell "c"))
    result `shouldBe` "abc"

--------------------------------------
-- Tests for Interaction with 'Let' --
--------------------------------------

testCBV :: Spec
testCBV = context "call-by-value" $ do
  it "writes output when the computation is bound by 'let_'" $ do
    let result = run (execWriter (runCBV @Tag (letTellComp @Tag)))
    result `shouldBe` "xyz"

testCBN :: Spec
testCBN = context "call-by-name" $ do
  it "writes output whenever the computation is used" $ do
    let result = run (execWriter (runCBN @Tag (letTellComp @Tag)))
    result `shouldBe` "zyxxyz"

testCBNeed :: Spec
testCBNeed = context "call-by-need" $ do
  it "writes output when the computation is used the first time" $ do
    let result = run (execWriter (final (runCBNeed @Tag (letTellComp @Tag))))
    result `shouldBe` "zyx"
