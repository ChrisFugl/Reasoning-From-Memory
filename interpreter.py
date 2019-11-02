from messages import COMMANDS, Command, Empty, Invalid, Valid

def interpret_command(user_input):
    preprocessed = preprocess(user_input)
    if len(preprocessed) == 0:
        return Empty()
    elif preprocessed in COMMANDS:
        return Command(preprocessed)
    else:
        return Invalid(preprocessed)

def interpret_fact(user_input):
    preprocessed = preprocess(user_input)
    if len(preprocessed) == 0:
        return Empty()
    elif preprocessed == 'invalid':
        return Invalid(preprocessed)
    else:
        return Valid(preprocessed, None)

def interpret_question(user_input):
    preprocessed = preprocess(user_input)
    if len(preprocessed) == 0:
        return Empty()
    elif preprocessed == 'invalid':
        return Invalid(preprocessed)
    else:
        return Valid(preprocessed, None)

def preprocess(text):
    """
    Preprocess text:
        * Removes leading and trailing whitespace.
        * ...
    """
    # remove leading and trailing whitespace
    preprocessed = text.strip()

    return preprocessed
