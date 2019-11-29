"""Contains function to match a triples with relations."""

from app.triple import create_triples
from app.types.match import Match

def find_matches(triples, threshold):
    """
    Find closest matching relation to every triple in a list.

    Matches are filtered such that only matches below the threshold are accepted.

    :type triples: list of app.types.triple.Triple
    :type threshold: float
    :rtype: list of app.types.match.Match
    """


def input2matches(user_input, threshold):
    """
    Transform preprocessed and validated user input to a list of matches.

    :type user_input: user_input
    :type threshold: float
    :rtype: list of app.types.match.Match
    """
    triples = create_triples(user_input)
    return find_matches(triples, threshold)
