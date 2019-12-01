import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux'
import { BrowserRouter as Router } from "react-router-dom";
import { createStore } from 'redux';
import { App } from 'web/components/App';
import { appReducer } from 'web/reducers';

const store = createStore(appReducer, {
  root: {},
});

const root = document.getElementById('app');
ReactDOM.render(
  <Provider store={store}>
    <Router>
      <App />
    </Router>
  </Provider>,
  root
);
