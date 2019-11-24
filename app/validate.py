"""Contain function to validate input."""

from app.types.command import Command
from app.types.validation import Validation

def validate_command(text):
    """
    Validate command.

    :type text: str
    :rtype: app.types.validation.Validation
    """
    if text in [Command.ASK, Command.BYE, Command.HELP, Command.TELL]:
        return Validation.VALID
    else:
        return Validation.UNKNOWN_COMMAND

def validate_user_input(text):
    """
    Validate user input. Assummes that user input is preprocessed.

    :type text: str
    :rtype: app.types.validation.Validation
    """
    if len(text) == 0:
        return Validation.EMPTY
    else:
        return Validation.VALID
