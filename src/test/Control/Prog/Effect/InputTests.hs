{-# LANGUAGE TypeApplications #-}

module Control.Prog.Effect.InputTests (testInputEffect) where

import           Test.Hspec                (Spec, context, describe, it,
                                            shouldBe)

import           Control.Prog              (run)
import           Control.Prog.Effect.Error (runError)
import           Control.Prog.Effect.Input (EndOfInput (EndOfInput), input,
                                            runInputFromList)

testInputEffect :: Spec
testInputEffect = describe "Control.Prog.Effect.Input" $ do
  testRunInputFromList

testRunInputFromList :: Spec
testRunInputFromList = context "runInputFromList" $ do
  it "reads input from the list" $ do
    let result = run (runError @EndOfInput (runInputFromList ["input"] input))
    result `shouldBe` Right "input"
  it "ignores additional input" $ do
    let result = run (runError @EndOfInput (runInputFromList ["input 1", "input 2"] input))
    result `shouldBe` Right "input 1"
  it "reads input from the left to right" $ do
    let result = run (runError @EndOfInput (runInputFromList ["input 1", "input 2"] (input >> input)))
    result `shouldBe` Right "input 2"
  it "throws an error when input is missing" $ do
    let result = run (runError @EndOfInput (runInputFromList [] input))
    result `shouldBe` Left EndOfInput
