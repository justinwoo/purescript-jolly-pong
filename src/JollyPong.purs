module JollyPong where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, runEffFn1, runEffFn3)
import Data.Function.Uncurried (Fn2)
import Data.Monoid (class Monoid)
import Data.Nullable (Nullable)
import Type.Prelude (class RowToList)
import Type.Row (Cons, Nil, kind RowList)

type Store e (state :: Type) (action :: Type) =
  { getState :: Eff e state
  , dispatch :: EffFn1 e action Unit
  , subscribe :: EffFn1 e (Listener e) (Dispose e)
  }

newtype Dispose e = Dispose (Eff e Unit)

newtype Reducer state action = Reducer (Fn2 (Nullable state) action state)

newtype Listener e = Listener (Eff e Unit)

newtype Middleware e1 e2 e3 state action = Middleware
  (Store e1 state action -> EffFn1 e2 action action -> EffFn1 e3 action action)

-- an action variant that is meant to be converted to whatever the
-- user needs. while this form is runtime compatible,
-- users should use libraries like purescript-variant
-- and decode this variant into Variant as needed.
newtype ActionVariant (actionRow :: # Type) = ActionVariant
  { type :: String
  }

createStore :: forall state action e
   . Reducer state action
  -> state
  -> Enhancer
  -> Eff e (Store e state action)
createStore = runEffFn3 _createStore

applyMiddleware :: forall action state e e' e''
   . Array (Middleware e e' e'' state action)
  -> Eff e Enhancer
applyMiddleware = runEffFn1 _applyMiddleware

-- combine reducers from a record of reducers
-- produces a correctly typed state row
-- users are expected to use the action variant as needed
combineReducers :: forall reducersRow stateRow actionRow rl
   . RowToList reducersRow rl
  => CombineReducers rl reducersRow stateRow actionRow
  => { | reducersRow }
  -> Reducer { | stateRow } (ActionVariant actionRow)
combineReducers = _combineReducers

-- class to take the rowlist of reducers record passed in
-- and prepare the state record row and a row of actions to be handled
class CombineReducers
  (rl :: RowList)
  (row :: # Type)
  (state :: # Type)
  (action :: # Type)
  | rl -> row state action

instance combineReducersCons ::
  ( CombineReducers tail row state' action'
  , RowCons name stateA state' state
  , Union actionA action' action
  ) => CombineReducers
    (Cons name (Reducer stateA (ActionVariant actionA)) tail)
    row
    state
    action

instance combineReducersNil :: CombineReducers Nil row () ()

foreign import data Enhancer :: Type
foreign import _createStore ::
  forall e state action initialState.
  EffFn3 e
    (Reducer state action)
    state
    Enhancer
    (Store e state action)
foreign import _combineReducers ::
  forall reducers state action
   . reducers
  -> Reducer state action

foreign import _applyMiddleware ::
  forall e middleware.
  EffFn1 e (Array middleware) Enhancer

