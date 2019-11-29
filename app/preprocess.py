"""Contain function to preprocess user input."""

import re

def preprocess(text):
    """
    Preprocess text.

    Removes leading and trailing whitespace.

    :type text: str
    :rtype: str
    """
    preprocessed = text
    # remove leading and trailing whitespace
    preprocessed = preprocessed.strip()
    # exactly one space between words
    space_pattern = r'([ \t][ \t]+)'
    preprocessed = re.sub(space_pattern, ' ', preprocessed)
    return preprocessed
