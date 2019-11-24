from app.interpreter import interpret_command, interpret_fact, interpret_question
from app.messages import Command, Empty, Invalid, Valid

def main():
    print('')
    print('| ------------- |')
    print('| Digital Brain |')
    print('| ------------- |')
    print('')
    print_help()
    while get_and_run_command():
        pass

def get_and_run_command():
    interpretation = prompt_command()
    if isinstance(interpretation, Empty):
        print_empty()
        return True
    elif isinstance(interpretation, Invalid):
        print_invalid()
        return True
    else:
        return do_command(interpretation.command)

def do_command(command):
    if command == 'exit':
        print('Bye!')
        return False
    elif command == 'help':
        print_help()
        return True
    elif command == 'ask':
        ask()
        return True
    elif command == 'tell':
        tell()
        return True

def ask():
    reply('What do you want to ask me?')
    interpretation = prompt_question()
    if isinstance(interpretation, Empty):
        print_empty()
    elif isinstance(interpretation, Invalid):
        print_invalid()
    else:
        # TODO: interpret questions
        reply('42')

def tell():
    reply('What do you want to tell me?')
    interpretation = prompt_fact()
    if isinstance(interpretation, Empty):
        print_empty()
    elif isinstance(interpretation, Invalid):
        print_invalid()
    else:
        # TODO: interpret facts
        reply('Got it!')

def prompt_command():
    user_input = input('> ')
    return interpret_command(user_input)

def prompt_fact():
    user_input = input('(tell) > ')
    return interpret_fact(user_input)

def prompt_question():
    user_input = input('(ask) > ')
    return interpret_question(user_input)

def print_empty():
    reply('Please tell me something.')

def print_help():
    print('---------------------------------------')
    print('Hi, I am your digital brain.')
    print('I can help you remember what you learn.')
    print('')
    print('I understand these directives:')
    print('* ask  --- ask me a question')
    print('* bye  --- stop me')
    print('* help --- see this message again')
    print('* tell --- tell me a fact')
    print('---------------------------------------')
    print('')

def print_invalid():
    reply(f'Sorry, but I do not understand that.')

def reply(message):
    print(f': {message}')

if __name__ == '__main__':
    main()
