import { CssBaseline, withStyles } from '@material-ui/core';
import React, { PureComponent } from 'react';
import { MainView } from 'web/components/MainView';

class App extends PureComponent {
  render() {
    const { classes } = this.props;
    return (
      <>
        <CssBaseline />
        <MainView />
      </>
    );
  }
}

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
