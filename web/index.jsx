import { createMuiTheme, ThemeProvider } from '@material-ui/core';
import { blueGrey, teal } from '@material-ui/core/colors';
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux'
import { createStore } from 'redux';
import { App } from 'web/components/App';
import { Connection } from 'web/containers/Connection';
import { appReducer } from 'web/reducers';

const store = createStore(appReducer, {
  chat: {
    messages: []
  },
  view: {
    type: 'load',
  },
});

const theme = createMuiTheme({
  palette: {
    primary: blueGrey,
    secondary: teal,
    type: 'dark',
  },
});

const root = document.getElementById('app');
ReactDOM.render(
  <Provider store={store}>
    <Connection>
      <ThemeProvider theme={theme}>
        <App />
      </ThemeProvider>
    </Connection>
  </Provider>,
  root
);
