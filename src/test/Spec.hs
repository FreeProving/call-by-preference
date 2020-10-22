module Spec (main) where

import           Test.Hspec                      (hspec)

import           Control.Prog.Effect.CutTests    (testCutEffect)
import           Control.Prog.Effect.ErrorTests  (testErrorEffect)
import           Control.Prog.Effect.FailTests   (testFailEffect)
import           Control.Prog.Effect.InputTests  (testInputEffect)
import           Control.Prog.Effect.NonDetTests (testNonDetEffect)
import           Control.Prog.Effect.OutputTests (testOutputEffect)
import           Control.Prog.Effect.ProbTests   (testProbEffect)
import           Control.Prog.Effect.ReaderTests (testReaderEffect)
import           Control.Prog.Effect.RefTests    (testRefEffect)
import           Control.Prog.Effect.StateTests  (testStateEffect)
import           Control.Prog.Effect.TraceTests  (testTraceEffect)
import           Control.Prog.Effect.WriterTests (testWriterEffect)
import           Control.Prog.Effect.YieldTests  (testYieldEffect)

main :: IO ()
main = hspec $ do
  testCutEffect
  testErrorEffect
  testFailEffect
  testInputEffect
  testNonDetEffect
  testOutputEffect
  testProbEffect
  testReaderEffect
  testRefEffect
  testStateEffect
  testTraceEffect
  testWriterEffect
  testYieldEffect
