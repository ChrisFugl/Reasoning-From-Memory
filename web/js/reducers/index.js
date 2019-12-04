import { combineReducers } from 'redux'

import { chatReducer } from './chat';
import { viewReducer } from './view';

const appReducer = combineReducers({
  chat: chatReducer,
  view: viewReducer,
});

export { appReducer };
