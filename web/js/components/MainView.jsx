import { withStyles } from '@material-ui/core';
import React, { PureComponent } from 'react';
import { connect } from 'react-redux';
import { Loading } from 'web/components/Loading';
import { ChatWindow } from 'web/components/ChatWindow';
import { withAPI } from 'web/containers/API';

class MainView extends PureComponent {
  componentDidMount() {
    if (this.props.view === 'load') {
      this.props.api.isLoading();
    }
  }

  render() {
    const view = this.getView();
    return (
      <div className={this.props.classes.root}>
        {view}
      </div>
    );
  }

  getView() {
    switch (this.props.view) {
      case 'load': return <Loading />;
      default: return <ChatWindow />;
    }
  }
}

function styles(theme) {
  return {
    root: {
      height: '100vh',
      width: '100vw',
    },
  };
}

function state2props(state) {
  return {
    view: state.view.type,
  };
}

const Styled = withStyles(styles)(MainView);
const WithAPI = withAPI(Styled);
const Connected = connect(state2props)(WithAPI);
export { Connected as MainView };
