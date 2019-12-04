const ADD_MESSAGE = 'reducers/chat/add-message';

export function addMessage(message, human) {
  return {
    type: ADD_MESSAGE,
    message,
    human,
  };
}

function addMessageReducer(state, action) {
  const message = {
    type: 'message',
    id: state.messages.length,
    message: action.message,
    human: action.human,
  };
  const messages = [...state.messages, message];
  return { ...state, messages };
}

const defaultState = {
  messages: [],
};

export function chatReducer(state = defaultState, action) {
  switch (action.type) {
    case ADD_MESSAGE: return addMessageReducer(state, action);
    default: return state;
  }
}
