const ASK = 'reducers/view/ask';
const LOAD = 'reducers/view/load';
const TELL = 'reducers/view/tell';

export function ask() {
  return {
    type: ASK,
  };
}

export function load() {
  return {
    type: LOAD,
  };
}

export function tell() {
  return {
    type: TELL,
  };
}

function askReducer(state, action) {
  return {
    ...state,
    type: 'ask',
  };
}

function loadReducer(state, action) {
  return {
    ...state,
    type: 'load',
  };
}

function tellReducer(state, action) {
  return {
    ...state,
    type: 'tell',
  };
}

const defaultState = {
  type: 'load',
};

export function viewReducer(state = defaultState, action) {
  switch (action.type) {
    case ASK: return askReducer(state, action);
    case LOAD: return loadReducer(state, action);
    case TELL: return tellReducer(state, action);
    default: return state;
  }
}
