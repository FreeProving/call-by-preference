{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Prog.Example.ML.TextIO
  ( -- * Input
    InStream
  , openIn
  , closeIn
  , inputAll
  , inputLine
  , endOfStream
    -- * Output
  , OutStream
  , openOut
  , openAppend
  , closeOut
  , output
  , flushOut
  )
where

import           Control.Prog                   ((:<:), Prog)
import           Control.Prog.Effect.InputFile  (InStream, InputFile)
import qualified Control.Prog.Effect.InputFile  as InputFile
import           Control.Prog.Effect.OutputFile (OutStream, OutputFile)
import qualified Control.Prog.Effect.OutputFile as OutputFile

-----------
-- Input --
-----------

openIn :: (InputFile :<: sig) => Prog sig FilePath -> Prog sig InStream
openIn pFilename = pFilename >>= InputFile.openIn

closeIn :: (InputFile :<: sig) => Prog sig InStream -> Prog sig ()
closeIn pInStream = pInStream >>= InputFile.closeIn

inputAll :: (InputFile :<: sig) => Prog sig InStream -> Prog sig String
inputAll pInStream = pInStream >>= InputFile.inputAll

inputLine :: (InputFile :<: sig) => Prog sig InStream -> Prog sig String
inputLine pInStream = pInStream >>= InputFile.inputLine

endOfStream :: (InputFile :<: sig) => Prog sig InStream -> Prog sig Bool
endOfStream pInStream = pInStream >>= InputFile.endOfStream

------------
-- Output --
------------

openOut :: (OutputFile :<: sig) => Prog sig FilePath -> Prog sig OutStream
openOut pFilename = pFilename >>= OutputFile.openOut

openAppend :: (OutputFile :<: sig) => Prog sig FilePath -> Prog sig OutStream
openAppend pFilename = pFilename >>= OutputFile.openAppend

closeOut :: (OutputFile :<: sig) => Prog sig OutStream -> Prog sig ()
closeOut pOutStream = pOutStream >>= OutputFile.closeOut

output :: (OutputFile :<: sig) => Prog sig OutStream -> Prog sig String -> Prog sig ()
output pOutStream pstr = pOutStream >>= \outStream -> pstr >>= OutputFile.output outStream

flushOut :: (OutputFile :<: sig) => Prog sig OutStream -> Prog sig ()
flushOut pOutStream = pOutStream >>= OutputFile.flushOut
