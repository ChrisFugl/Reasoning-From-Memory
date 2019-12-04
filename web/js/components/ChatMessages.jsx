import { withStyles } from '@material-ui/core';
import React, { PureComponent } from 'react';
import { connect } from 'react-redux';
import { ChatMessage } from 'web/components/ChatMessage';
import { CHAT_INPUT_HEIGHT } from 'web/constants';

class ChatMessages extends PureComponent {
  constructor(props) {
    super(props);
    this.bottomScrollElementRef = React.createRef();
    this.renderMessage = this.renderMessage.bind(this);
  }

  componentDidMount() {
    this.scrollToBottom();
  }

  componentDidUpdate() {
    this.scrollToBottom();
  }

  scrollToBottom() {
    this.bottomScrollElementRef.current.scrollIntoView({ behavior: 'smooth' });
  }

  render() {
    const { classes, messages } = this.props;
    return (
      <div className={classes.container}>
        {messages.map(this.renderMessage)}
        <div ref={this.bottomScrollElementRef} />
      </div>
    );
  }

  renderMessage(message) {
    return <ChatMessage key={message.id} {...message} />;
  }
}

function styles(theme) {
  return {
    container: {
      alignItems: 'flex-start',
      display: 'flex',
      flexDirection: 'column',
      height: `calc(100% - ${CHAT_INPUT_HEIGHT}px)`,
      overflowX: 'hidden',
      overflowY: 'scroll',
      padding: 12,
      position: 'relative',
    },
  };
}

function state2props(state) {
  return {
    messages: state.chat.messages,
  };
}

const Styled = withStyles(styles)(ChatMessages);
const Connected = connect(state2props)(Styled);

export { Connected as ChatMessages };
