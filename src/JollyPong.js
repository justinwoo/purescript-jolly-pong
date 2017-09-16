var redux = require('redux');

exports._createStore = redux.createStore;

exports._combineReducers = redux.combineReducers;

exports._applyMiddleware = function (middlewares) {
  return redux.applyMiddleware.apply(this, middlewares);
};

