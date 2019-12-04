import { Button, InputBase, Paper, withStyles } from '@material-ui/core';
import React, { PureComponent } from 'react';
import { CHAT_INPUT_HEIGHT } from 'web/constants';
import { withAPI } from 'web/containers/API';

class ChatInput extends PureComponent {
  constructor(props) {
    super(props);
    this.state = {
      inputClasses: {
        input: props.classes.input,
        root: props.classes.inputRoot,
      },
      value: '',
    };
    this.ask = this.ask.bind(this);
    this.onChange = this.onChange.bind(this);
    this.onSubmit = this.onSubmit.bind(this);
    this.tell = this.tell.bind(this);
  }

  onChange(event) {
    this.setState({
      value: event.target.value,
    });
  }

  onSubmit(event) {
    event.preventDefault();
    event.stopPropagation();
  }

  ask(event) {
    this.onSubmit(event);
    this.props.api.askQuestion(this.state.value);
    this.clear();
  }

  tell(event) {
    this.onSubmit(event);
    this.props.api.tellFact(this.state.value);
    this.clear();
  }

  clear() {
    this.setState({
      value: '',
    });
  }

  render() {
    const { classes } = this.props;
    const placeholder = 'Ask me a question or tell me a fact.';
    return (
      <Paper component='form' className={classes.container} onSubmit={this.onSubmit} elevation={0}>
        <InputBase classes={this.state.inputClasses} placeholder={placeholder} onChange={this.onChange} value={this.state.value} />
          {this.renderSubmit('Ask', this.ask)}
          {this.renderSubmit('Tell', this.tell)}
      </Paper>
    );
  }

  renderSubmit(title, action) {
    const { classes } = this.props;
    const disabled = this.state.value.replace(/\s/g, '').length === 0;
    return <Button className={classes.button} type='button' onClick={action} disabled={disabled}>{title}</Button>;
  }
}

function styles(theme) {
  return {
    button: {
      borderRadius: 0,
      width: CHAT_INPUT_HEIGHT,
      height: '100%',
      ['&:last-child']: {
        borderBottomRightRadius: 12,
      },
    },
    container: {
      borderTop: `1px solid ${theme.palette.divider}`,
      borderTopLeftRadius: 0,
      borderTopRightRadius: 0,
      borderBottomLeftRadius: 12,
      borderBottomRightRadius: 12,
      display: 'flex',
      alignItems: 'center',
      width: '100%',
      height: CHAT_INPUT_HEIGHT,
    },
    input: {
      border: 'none',
      borderRadius: 0,
      height: CHAT_INPUT_HEIGHT - 2 * 16,
      width: '100%',
      padding: 16,
    },
    inputRoot: {
      width: '100%',
    }
  };
}

const Styled = withStyles(styles)(ChatInput);
const WithAPI = withAPI(Styled);

export { WithAPI as ChatInput };
