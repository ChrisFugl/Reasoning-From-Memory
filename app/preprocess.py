"""Contain function to preprocess user input."""

def preprocess(text):
    """
    Preprocess text.

    Removes leading and trailing whitespace.

    :type text: str
    :rtype: str
    """
    # remove leading and trailing whitespace
    preprocessed = text.strip()
    return preprocessed
