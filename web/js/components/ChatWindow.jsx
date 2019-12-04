import { withStyles } from '@material-ui/core';
import React, { PureComponent } from 'react';
import { ChatInput } from 'web/components/ChatInput';
import { ChatMessages } from 'web/components/ChatMessages';

class ChatWindow extends PureComponent {
  render() {
    const { classes } = this.props;
    return (
      <div className={classes.container}>
        <div className={classes.window}>
          <ChatMessages />
          <ChatInput />
        </div>
      </div>
    );
  }
}

function styles(theme) {
  return {
    container: {
      alignItems: 'center',
      bacground: theme.palette.background.deafult,
      display: 'flex',
      justifyContent: 'center',
      height: '100%',
      width: '100%',
    },
    window: {
      background: theme.palette.background.paper,
      border: `1px solid ${theme.palette.primary.main}`,
      borderRadius: 12,
      width: '100%',
      maxWidth: 720,
      height: '100%',
      maxHeight: 720,
      boxShadow: theme.shadows[2],
    },
  };
}

const Styled = withStyles(styles)(ChatWindow);

export { Styled as ChatWindow };
