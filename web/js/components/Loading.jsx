import { CircularProgress, withStyles } from '@material-ui/core';
import React, { PureComponent } from 'react';

class Loading extends PureComponent {
  render() {
    return (
      <div className={this.props.classes.container}>
        <CircularProgress size={72} />
      </div>
    );
  }
}

function styles(theme) {
  return {
    container: {
      alignItems: 'center',
      display: 'flex',
      justifyContent: 'center',
      height: '100%',
      width: '100%',
    },
  };
}


const Styled = withStyles(styles)(Loading);
export { Styled as Loading };
