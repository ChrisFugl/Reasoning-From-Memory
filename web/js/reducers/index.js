import { combineReducers } from 'redux'


function root(state = {}, action) {
  return {};
}

const appReducer = combineReducers({ root });

export { appReducer };
