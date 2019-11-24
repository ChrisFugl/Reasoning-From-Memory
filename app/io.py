"""Contains methods to interact with user."""

from app.parse import parse

def exit():
    """Print exit message."""
    print('Bye!')

def help():
    """Print help message."""
    print('---------------------------------------')
    print('')
    print('I understand these directives:')
    print('* ask  --- ask me a question')
    print('* bye  --- stop me')
    print('* help --- see this message again')
    print('* tell --- tell me a fact')
    print('---------------------------------------')
    print('')

def prompt(prefix=None):
    """
    Prompt the user for input.

    Input is parsed.

    :type prefix: str
    :rtype: (str, bool, str)
    """
    if prefix is None:
        user_input = input('> ')
    else:
        user_input = input(f'({prefix}) > ')
    return parse(user_input)

def reply(message):
    """
    Print reply to user.

    :type message: str
    """
    print(f': {message}')

def welcome():
    """Print welcome message."""
    print('')
    print('| ------------- |')
    print('| Digital Brain |')
    print('| ------------- |')
    print('')
    print('Hi, I am your digital brain.')
    print('I can help you remember what you learn.')
    print('')
