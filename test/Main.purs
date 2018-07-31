module Test.Main where

import Prelude

import Data.Function.Uncurried (mkFn2)
import Data.Maybe (fromMaybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried as EU
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

middleware :: Middleware MyState MyAction
middleware = Middleware $ \store -> \next -> EU.mkEffectFn1 \action -> do
  log $ "action called: " <> unsafeStringify action
  EU.runEffectFn1 next action

main :: Effect Unit
main = do
  enhancer <- applyMiddleware [middleware]
  store <- createStore reducer initialState enhancer
  Dispose dispose <- EU.runEffectFn1 store.subscribe $ listener store
  _ <- EU.runEffectFn1 store.dispatch $ ActionVariant {type: "asdf"}
  _ <- EU.runEffectFn1 store.dispatch $ ActionVariant {type: "asdf"}
  dispose
  where
    listener store = Listener do
      state <- store.getState
      log $ "listener called! state: " <> unsafeStringify state
