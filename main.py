"""Run this script to start the digital brain."""

from app.ask import ask
import app.io as io
from app.memories.persistent import PersistentMemory
from app.tell import tell
from app.types.command import Command
from app.types.validation import Validation
from app.validate import validate_command

THRESHOLD_FACT = 1
THRESHOLD_QUESTION = 1

def _main():
    io.welcome()
    io.help()
    memory = PersistentMemory()
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
                    ask(memory.facts, THRESHOLD_QUESTION)
                elif user_input == Command.TELL:
                    tell(memory, THRESHOLD_FACT)

if __name__ == '__main__':
    _main()
