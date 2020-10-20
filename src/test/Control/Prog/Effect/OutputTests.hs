module Control.Prog.Effect.OutputTests (testOutputEffect) where

import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe)

import           Control.Prog               (run)
import           Control.Prog.Effect.Output (output, runOutputToList)

testOutputEffect :: Spec
testOutputEffect = describe "Control.Prog.Effect.Output" $ do
  testRunOutputToList

testRunOutputToList :: Spec
testRunOutputToList = context "runOutputToList" $ do
  it "collects output in a list" $ do
    let result = run (runOutputToList (output "output"))
    result `shouldBe` (["output"], ())
  it "collects output in the right order" $ do
    let result = run (runOutputToList (output "output 1" >> output "output 2"))
    result `shouldBe` (["output 1", "output 2"], ())
