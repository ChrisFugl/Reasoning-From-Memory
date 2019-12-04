import React, { PureComponent } from 'react';
import { ConnectionContext } from 'web/containers/Connection';

function withAPI(WrappedComponent) {
  class Wrapper extends PureComponent {
    render() {
      return (
        <ConnectionContext.Consumer>
          {(context) => <WrappedComponent {...this.props} {...context} />}
        </ConnectionContext.Consumer>
      );
    }
  }

  return Wrapper;
}

export { withAPI };
