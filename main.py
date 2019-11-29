"""Run this script to start the digital brain."""

import argparse
from app.ask import ask
import app.io as io
from app.memories.persistent import PersistentMemory
from app.tell import tell
from app.types.command import Command
from app.types.validation import Validation
from app.validate import validate_command
import os
import json
import sys

def _main():
    arguments = sys.argv[1:]
    options = _parse_arguments(arguments)
    config = _parse_config(options.config)
    thresholds = config['threholds']
    memory_path = os.path.abspath(config['memory_path'])
    memory = PersistentMemory(memory_path)
    io.welcome()
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
                    ask(memory.facts, thresholds['question'])
                elif user_input == Command.TELL:
                    tell(memory, thresholds['fact'])

def _parse_arguments(arguments):
    parser = argparse.ArgumentParser()
    parser.add_argument('config', type=str, help='path to config')
    options = parser.parse_args(arguments)
    return options

def _parse_config(config_path):
    with open(config_path, 'r') as config_file:
        config = json.load(config_file)
    return config

if __name__ == '__main__':
    _main()
