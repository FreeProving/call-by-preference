{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Control.Prog.Util.Upcast (upcast) where

import           Control.Prog.Class     (type (-->), HInvariant (hinvmap),
                                         SigFunctor)
import           Control.Prog.Prog      (Prog (Op, Var))
import           Control.Prog.Signature ((:+:) (Inr))

upcast
  :: (SigFunctor sig', SigFunctor sig, HInvariant sig)
  => (Prog (sig' :+: sig) --> Prog sig)
  -> Prog sig --> Prog (sig' :+: sig)
upcast _    (Var x  ) = Var x
upcast down (Op  sig) = Op (Inr (hinvmap (upcast down) down sig))
