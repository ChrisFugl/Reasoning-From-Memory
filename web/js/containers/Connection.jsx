import React, { PureComponent } from 'react';
import { connect } from 'react-redux';
import { addMessage } from 'web/reducers/chat';
import { ask, load } from 'web/reducers/view';

const ConnectionContext = React.createContext();
ConnectionContext.displayName = 'ConnectionContext';

const messages = {
  no_match: [
    'Sorry. I do not understand that.',
    'My digital brain is not yet fully evolved. I am afraid I did not understand what you said.',
    'I do not understand. Can you rephrase it?',
  ],
  stored: [
    'Got it!',
    'I will remember that.',
    'Saved to my internal memory.',
  ],
  welcome: [
    'Hi, I am your digital brain. I can help you remember what you learn.'
  ],
};

class Connection extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      open: false,
      hasSentWelcomeMessage: false,
    };
    this.websocket = new WebSocket('ws://localhost:10579')
    this.websocket.onopen = this.onOpen.bind(this);
    this.websocket.onmessage = this.onMessage.bind(this);
    this.api = {
      api: {
        askQuestion: this.askQuestion.bind(this),
        isLoading: this.isLoading.bind(this),
        tellFact: this.tellFact.bind(this),
      },
    };
  }

  componentWillUnmount() {
    this.websocket.close();
  }

  onMessage(event) {
    const data = JSON.parse(event.data);
    switch (data.type) {
      case 'loading': this.props.load(); break;
      case 'finished_loading':
        this.props.ask();
        if (!this.state.hasSentWelcomeMessage) {
          this.welcome();
        }
        break;
      case 'invalid': this.reply([data.message]); break;
      case 'no_match': this.reply(messages.no_match); break;
      case 'select_match': throw Exception('select_match is not implemented yet'); break;
      case 'answer': this.reply([data.answer]); break;
      case 'stored': this.reply(messages.stored); break;
    }
  }

  onOpen(event) {
    this.setState({
      open: true,
    });
  }

  askQuestion(question) {
    this.props.addMessage(question, true);
    const message = {
      type: 'ask',
      message: question,
    };
    this.request(message);
  }

  isLoading() {
    const message = {
      type: 'is_loading',
    };
    this.request(message);
  }

  tellFact(fact) {
    this.props.addMessage(fact, true);
    const message = {
      type: 'tell',
      message: fact,
    };
    this.request(message);
  }

  request(message) {
    const request = JSON.stringify(message);
    this.websocket.send(request);
  }

  reply(messages) {
    const message = this.getMessage(messages);
    setTimeout(() => this.props.addMessage(message, false), 250);
  }

  getMessage(messages) {
    const message_index = this.randomIntegerInRange(0, messages.length);
    const message = messages[message_index];
    return message;
  }

  randomIntegerInRange(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
  }

  welcome() {
    const message = this.getMessage(messages.welcome);
    this.props.addMessage(message, false);
    this.setState({
      hasSentWelcomeMessage: true,
    });
  }

  render() {
    if (!this.state.open) {
      return null;
    }
    return (
      <ConnectionContext.Provider value={this.api}>
        {this.props.children}
      </ConnectionContext.Provider>
    );
  }
}

function state2props(state) {
  return {};
}

const dispatch2props = {
  addMessage,
  ask,
  load,
}

const ConnectedToStore = connect(state2props, dispatch2props)(Connection);

export {
  ConnectedToStore as Connection,
  ConnectionContext,
};
