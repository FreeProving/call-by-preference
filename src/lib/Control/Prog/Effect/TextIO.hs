{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Prog.Effect.TextIO
  ( -- * Effect
    OutputFile
  , OutStream
    -- * Actions
  , openOut
  , closeOut
  , output
    -- * Handlers
  )
where


import           Control.Prog.Effect.OutputFile (OutStream, OutputFile)
import qualified Control.Prog.Effect.OutputFile as OutputFile

import           Control.Prog.Prog              (Prog)
import           Control.Prog.Signature         ((:<:))


-------------
-- Actions --
-------------

openOut :: (OutputFile :<: sig) => Prog sig FilePath -> Prog sig OutStream
openOut pfilename = pfilename >>= OutputFile.openOut

closeOut :: (OutputFile :<: sig) => Prog sig OutStream -> Prog sig ()
closeOut poutStream = poutStream >>= OutputFile.closeOut

output :: (OutputFile :<: sig) => Prog sig OutStream -> Prog sig String -> Prog sig ()
output poutStream pstr = poutStream >>= \outStream -> pstr >>= OutputFile.output outStream
