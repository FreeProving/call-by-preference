module Examples (main) where

import           Test.Hspec                      (hspec)

import           Control.Prog.Example.ML (testMLExamples)

main :: IO ()
main = hspec $ do
  testMLExamples
