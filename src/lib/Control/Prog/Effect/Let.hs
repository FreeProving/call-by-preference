{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Control.Prog.Effect.Let
  ( -- * Effect
    Let (Let)
    -- * Actions
  , let_
    -- * Call-By-Value Handlers
  , runCBV
    -- * Call-By-Name Handlers
  , runCBN
    -- * Call-By-Need Handlers with 'State' Effect
  , runCBNeed
  , final
    -- * Call-By-Need Handlers with 'Ref' Effect
  , IDRef
  , runCBNeedWithRef
  , finalWithRef
  )
where

import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import           Data.Typeable             (Typeable, cast)

import           Control.Prog.Class        (HInvariant (hinvmap),
                                            SigFunctor (sigmap), Syntax, hmap')
import           Control.Prog.Effect.Ref   (Ref (NewRef, ReadRef, WriteRef),
                                            newRef, readRef, writeRef)
import           Control.Prog.Effect.State (State, evalState, get, gets, modify,
                                            put)
import           Control.Prog.Prog         (Prog (Op, Var))
import           Control.Prog.Signature    ((:+:) (Inl, Inr), (:<:), inject)
import           Control.Prog.Util.Upcast  (upcast)

------------
-- Effect --
------------

data Let tag m a = forall x. (Typeable x) => Let (m x) (m x -> m a)

instance SigFunctor (Let tag) where
  sigmap f (Let mx k) = Let mx (f . k)

instance HInvariant (Let tag) where
  hinvmap f g (Let mx k) = Let (f mx) (f . k . g)

-------------
-- Actions --
-------------

let_
  :: forall tag a sig
   . (Let tag :<: sig, Typeable a)
  => Prog sig a
  -> Prog sig (Prog sig a)
let_ fx = inject (Let @tag fx return)

-------------------
-- Call-By-Value --
-------------------

runCBV
  :: forall tag sig a
   . (SigFunctor sig, HInvariant sig)
  => Prog (Let tag :+: sig) a
  -> Prog sig a
runCBV (Var x                 ) = return x
runCBV (Op  (Inl (Let fx k)))   = runCBV fx >>= \x -> runCBV (k (return x))
runCBV (Op  (Inr sig         )) = Op (hinvmap runCBV (upcast runCBV) sig)

------------------
-- Call-By-Name --
------------------

runCBN
  :: forall tag a sig
   . (SigFunctor sig, HInvariant sig)
  => Prog (Let tag :+: sig) a
  -> Prog sig a
runCBN (Var x                 ) = return x
runCBN (Op  (Inl (Let fx k)))   = runCBN (k fx)
runCBN (Op  (Inr sig         )) = Op (hinvmap runCBN (upcast runCBN) sig)

--------------------------------------
-- Call-By-Need with 'State' Effect --
--------------------------------------

runCBNeed
  :: forall tag a sig
   . ( HInvariant sig
     , State ValueStore :<: sig
     , State ScopeID :<: sig
     )
  => Prog (Let tag :+: sig) a
  -> Prog sig a
runCBNeed (Var x               ) = return x
runCBNeed (Op  (Inl (Let fx k))) = do
  sID <- get
  put (nextScopeID sID)
  runCBNeed $ k do
    store <- get
    case lookupValue sID store of
      Just v  -> return v
      Nothing -> do
        x      <- fx
        store' <- get
        put (insertValue sID x store')
        return x
runCBNeed (Op (Inr sig)) = Op (hinvmap runCBNeed (upcast runCBNeed) sig)

final
  :: forall a sig
   . (Syntax sig)
  => Prog (State ScopeID :+: State ValueStore :+: sig) a
  -> Prog sig a
final fx = evalState emptyValueStore (evalState initialScopeID fx)

------------------------------------
-- Call-By-Need with 'Ref' Effect --
------------------------------------

runCBNeedWithRef
  :: forall loc tag sig a
   . (HInvariant sig, Ref loc :<: sig)
  => Prog (Let tag :+: sig) a
  -> Prog sig a
runCBNeedWithRef (Var x)               = return x
runCBNeedWithRef (Op (Inl (Let fx k))) = do
  r <- newRef @loc Nothing
  runCBNeedWithRef @loc $ k $ do
    mv <- readRef @loc r
    case mv of
      Just v -> return v
      Nothing  -> do
        x <- fx
        writeRef @loc r (Just x)
        return x
runCBNeedWithRef (Op (Inr sig)) =
  Op (hinvmap (runCBNeedWithRef @loc) (upcast (runCBNeedWithRef @loc)) sig)

finalWithRef :: (Syntax sig) => Prog (Ref IDRef :+: sig) a -> Prog sig a
finalWithRef fx = final (runIDRef fx)

---------------
-- Scope IDs --
---------------

newtype ScopeID = ScopeID { counter :: Int }
 deriving (Eq, Ord, Show)

initialScopeID :: ScopeID
initialScopeID = ScopeID 0

nextScopeID :: ScopeID -> ScopeID
nextScopeID = ScopeID . (+1) . counter

-------------------------
-- Scope ID References --
-------------------------

data IDRef a = IDRef ScopeID

runIDRef :: (Syntax sig) => Prog (Ref IDRef :+: sig) a -> Prog (State ScopeID :+: State ValueStore :+: sig) a
runIDRef (Var x)                               = return x
runIDRef (Op (Inl (NewRef b k)))               = do sID <- get; put (nextScopeID sID); modify (insertValue sID b); runIDRef (k (IDRef sID))
runIDRef (Op (Inl (ReadRef (IDRef sID) k)))    = do
  -- Since an 'IDRef' can only be created using 'newRef' and 'newRef' always
  -- inserts the initial value into the value store, we can be sure that
  -- the lookup does not return 'Nothing'.
  value <- gets (fromJust . lookupValue sID)
  runIDRef (k value)
runIDRef (Op (Inl (WriteRef (IDRef sID) b v))) = do
  modify (insertValue sID b)
  runIDRef v
runIDRef (Op (Inr sig))                        = Op (hmap' runIDRef (Inr (Inr sig)))

-----------------
-- Value Store --
-----------------

data Value = forall a. (Typeable a) => Value a

newtype ValueStore = ValueStore { valueStoreMap :: Map ScopeID Value }

emptyValueStore :: ValueStore
emptyValueStore = ValueStore Map.empty

insertValue :: (Typeable a) => ScopeID -> a -> ValueStore -> ValueStore
insertValue key val = ValueStore . Map.insert key (Value val) . valueStoreMap

lookupValue :: (Typeable a) => ScopeID -> ValueStore -> Maybe a
lookupValue key store = case Map.lookup key (valueStoreMap store) of
  Just (Value val) -> cast val
  Nothing          -> Nothing
