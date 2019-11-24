"""Contains function to parse user input."""

from app.preprocess import preprocess
from app.types.validation import Validation
from app.validate import validate_user_input

def parse(user_input):
    """
    Preprocess and validate user input.

    :type user_input: str
    :rtype: (str, bool, str)
    """
    preprocessed = preprocess(user_input)
    validated = validate_user_input(preprocessed)
    valid = validated == Validation.VALID
    validation_message = None
    if not valid:
        validation_message = 'Please tell me something.'
    return preprocessed, valid, validation_message
