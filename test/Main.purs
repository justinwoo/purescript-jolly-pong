module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (mkEffFn1, runEffFn1)
import Data.Function.Uncurried (mkFn2)
import Data.Maybe (fromMaybe)
import Data.Nullable (toMaybe)
import Global.Unsafe (unsafeStringify)
import JollyPong (ActionVariant(..), Dispose(..), Listener(Listener), Middleware(Middleware), Reducer(Reducer), applyMiddleware, combineReducers, createStore)

type MyState =
  { a :: Int
  , b :: String
  }

type MyAction = ActionVariant ("asdf" :: Unit)

aReducer :: Reducer Int (ActionVariant ("asdf" :: Unit))
aReducer = Reducer $ mkFn2 \a b -> fromMaybe 1 $ add 1 <$> toMaybe a

bReducer :: Reducer String (ActionVariant ())
bReducer = Reducer $ mkFn2 \a b -> "sdfd"

reducer :: Reducer MyState MyAction
reducer = combineReducers
  { a: aReducer
  , b: bReducer
  }

initialState :: {a :: Int}
initialState =
  { a: 1
  }

middleware :: Middleware _ _ _ MyState MyAction
middleware = Middleware $ \store -> \next -> mkEffFn1 \action -> do
  log $ "action called: " <> unsafeStringify action
  runEffFn1 next action

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  enhancer <- applyMiddleware [middleware]
  store <- createStore reducer initialState enhancer
  Dispose dispose <- runEffFn1 store.subscribe $ listener store
  _ <- runEffFn1 store.dispatch $ ActionVariant {type: "asdf"}
  _ <- runEffFn1 store.dispatch $ ActionVariant {type: "asdf"}
  dispose
  log "You should add some tests."
  where
    listener store = Listener do
      state <- store.getState
      log $ "listener called! state: " <> unsafeStringify state
