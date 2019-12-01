"""Run this script to start the digital brain."""

from app.ask import ask
import app.io as io
from app.language import get_language
from app.match import Matcher
from app.memories.persistent import PersistentMemory
from app.tell import tell
from app.types.command import Command
from app.types.validation import Validation
from app.validate import validate_command
import os
import json

_CONFIG_PATH = 'configs/shell.json'

def _main():
    io.welcome()
    config = _parse_config()
    thresholds = config['threholds']
    memory_path = os.path.abspath(config['memory_path'])
    memory = PersistentMemory(memory_path)
    nlp = get_language()
    matcher = Matcher(nlp)
    io.help()
    while True:
        user_input, valid, validation_mesage = io.prompt()
        if not valid:
            io.reply(validation_mesage)
        else:
            command_validation_result = validate_command(user_input)
            if command_validation_result == Validation.UNKNOWN_COMMAND:
                io.reply('I am sorry, but I do not understand that.')
            else:
                if user_input == Command.BYE:
                    io.exit()
                    break
                elif user_input == Command.HELP:
                    io.help()
                elif user_input == Command.ASK:
                    _ask(thresholds['question'], matcher, memory.facts)
                elif user_input == Command.TELL:
                    _tell(thresholds['fact'], matcher, memory)

def _parse_config():
    with open(_CONFIG_PATH, 'r') as config_file:
        config = json.load(config_file)
    return config

def _ask(threshold, matcher, facts):
    """
    Interactive option to ask a question.

    :type threshold: float
    :type matcher: app.match.Matcher
    :type facts: list of app.types.fact.Fact
    """
    io.reply('What do you want to ask me?')
    while True:
        message = io.prompt_without_parse('ask')
        response = ask(threshold, matcher, facts, message)
        type = response['type']
        if type == 'invalid':
            io.reply(response['validation_mesage'])
        elif type == 'no_match':
            # TODO: how to give detailed information on why no match was found?
            io.reply('Sorry. My digital brain is not yet fully evolved. I did not understand it.')
            io.reply('Could you rephrase the question, so I might be able to understand it?')
            return
        elif type == 'select_match':
             _select_match(response['matches'])
        else:
            io.reply(response['answer'])
            return

def _select_match(matches):
    """
    Present all possible matches to user and prompt user to pick one of them.

    :type matches: list of app.types.match.Match
    :rtype: app.types.match.Match
    """
    # TOOD: add user friendly representation to of each relation
    raise NotImplementedError()

def _tell(threshold, matcher, memory):
    io.reply('What do you want to tell me?')
    while True:
        message = io.prompt_without_parse('tell')
        response = tell(threshold, matcher, memory, message)
        if type == 'invalid':
            io.reply(response['validation_mesage'])
        elif type == 'no_match':
            # TODO: how to give detailed information on why no match was found?
            io.reply('Sorry. My digital brain is not yet fully evolved. I did not understand it.')
            io.reply('Could you rephrase the question, so I might be able to understand it?')
            return
        else:
            io.reply('Got it! I will remember that.')
            return

if __name__ == '__main__':
    _main()
