import { CssBaseline } from '@material-ui/core';
import React, { Component } from 'react';
import { Routes } from 'web/components/Routes';

export class App extends Component {
  render() {
    const { classes } = this.props;
    return (
      <>
        <CssBaseline />
        <Routes />
      </>
    );
  }
}
