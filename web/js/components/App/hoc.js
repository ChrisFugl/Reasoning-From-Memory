import { withStyles } from '@material-ui/core';

import { App } from './App';

function style(theme) {
  return {
    root: {
      height: '100vh',
      width: '100vw',
    },
  };
}

const Styled = withStyles(style)(App);
export { Styled as App };
