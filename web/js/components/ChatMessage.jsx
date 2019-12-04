import { Typography, withStyles } from '@material-ui/core';
import classnames from 'classnames';
import React, { PureComponent } from 'react';

class ChatMessage extends PureComponent {
  render() {
    const { type } = this.props;
    if (type === 'message') {
      return this.renderMessage();
    }
  }

  renderMessage() {
    const { classes, message, human } = this.props;
    const className = classnames(classes.container, {
      [classes.computer]: !human,
      [classes.human]: human,
    });
    return (
      <div className={className}>
        <Typography>{message}</Typography>
      </div>
    );
  }
}

function styles(theme) {
  return {
    computer: {
      background: theme.palette.primary.dark,
    },
    human: {
      background: theme.palette.secondary.dark,
      marginLeft: 'auto',
      textAlign: 'right',
    },
    container: {
      border: `1px solid ${theme.palette.divider}`,
      boxShadow: theme.shadows[1],
      color: '#fff',
      display: 'inline-block',
      borderRadius: 6,
      position: 'relative',
      minWidth: 24,
      maxWidth: 336,
      width: 'auto',
      marginBottom: 16,
      padding: '8px 12px',
      ['&:last-child']: {
        marginBottom: 0,
      }
    },
  };
}

const Styled = withStyles(styles)(ChatMessage);

export { Styled as ChatMessage };
