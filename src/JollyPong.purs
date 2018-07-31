module JollyPong where

import Prelude

import Data.Function.Uncurried as FU
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried as EU
import Prim.Row as Row
import Type.Prelude (class RowToList)
import Type.Row (Cons, Nil, kind RowList)

-- | A Redux Store.
type Store (state :: Type) (action :: Type) =
  { getState :: Effect state
  , dispatch :: EU.EffectFn1 action Unit
  , subscribe :: EU.EffectFn1 Listener Dispose
  }

-- | A dispose function to unsubscribe from a store.
newtype Dispose = Dispose (Effect Unit)

-- | A reducer. Note that the initial state in Redux is actually an `undefined` value.
newtype Reducer state action = Reducer (FU.Fn2 (Nullable state) action state)

-- | A listener for Redux store subscriptions.
newtype Listener = Listener (Effect Unit)

-- | Middleware for Redux.
newtype Middleware state action = Middleware
  (Store state action -> EU.EffectFn1 action action -> EU.EffectFn1 action action)

-- | an action variant that is meant to be converted to whatever the
-- | user needs. while this form is runtime compatible,
-- | users should use libraries like purescript-variant
-- | and decode this variant into Variant as needed.
newtype ActionVariant (actionRow :: # Type) = ActionVariant
  { type :: String
  }

-- | Create a store using the reducer, a subrow of the state, and enhancers.
createStore :: forall state initialState state' action
   . Row.Union initialState state' state
  => Reducer {|state} action
  -> {|initialState}
  -> Enhancer
  -> Effect (Store {|state} action)
createStore = EU.runEffectFn3 _createStore

-- | Apply middleware.
applyMiddleware :: forall action state
   . Array (Middleware state action)
  -> Effect Enhancer
applyMiddleware = EU.runEffectFn1 _applyMiddleware

-- | combine reducers from a record of reducers
-- | produces a correctly typed state row
-- | users are expected to use the action variant as needed
combineReducers :: forall reducersRow stateRow actionRow rl
   . RowToList reducersRow rl
  => CombineReducers rl reducersRow stateRow actionRow
  => {|reducersRow }
  -> Reducer {|stateRow } (ActionVariant actionRow)
combineReducers = _combineReducers

-- | class to take the rowlist of reducers record passed in
-- | and prepare the state record row and a row of actions to be handled
class CombineReducers
  (rl :: RowList)
  (row :: # Type)
  (state :: # Type)
  (action :: # Type)
  | rl -> row state action

instance combineReducersCons ::
  ( CombineReducers tail row state' action'
  , Row.Cons name stateA state' state
  , Row.Union actionA action' action
  ) => CombineReducers
    (Cons name (Reducer stateA (ActionVariant actionA)) tail)
    row
    state
    action

instance combineReducersNil :: CombineReducers Nil row () ()

foreign import data Enhancer :: Type
foreign import _createStore ::
  forall state action initialState.
  EU.EffectFn3
    (Reducer state action)
    initialState
    Enhancer
    (Store state action)
foreign import _combineReducers ::
  forall reducers state action
   . reducers
  -> Reducer state action

foreign import _applyMiddleware ::
  forall middleware.
  EU.EffectFn1 (Array middleware) Enhancer
