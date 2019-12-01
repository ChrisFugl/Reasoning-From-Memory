import React, { Component } from 'react';
import { Route, Switch } from 'react-router';
import { routes } from 'web/pages';

function route2component(route) {
  return <Route key={route.path} exact={route.exact} component={route.component} path={route.path} />;
}

export class Routes extends Component {
  constructor(props) {
    super(props);
    this.state = {
      routes: routes.map(route2component),
    }
  }

  render() {
    return (
      <Switch>
        {this.state.routes}
      </Switch>
    );
  }
}
