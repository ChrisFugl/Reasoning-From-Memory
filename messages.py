COMMANDS = [
    'bye',
    'help',
    'tell',
    'ask'
]

class Command:

    def __init__(self, command):
        self.command = command

    def __str__(self):
        return f'Command: {self.command}'

class Valid:

    def __init__(self, text, representation):
        self.text = text
        self.representation = representation

    def __str__(self):
        return f'Fact: "{self.text}"'

class Invalid:

    def __init__(self, text):
        self.text = text

    def __str__(self):
        return f'Invalid: "{self.text}"'

class Empty:

    def __str__(self):
        return 'Empty'
