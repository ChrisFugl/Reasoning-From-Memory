"""Contains function to match a triples with relations."""

from app.distance import distance as get_distance
from app.triple import create_triples
from app.types.match import Match
from app.utils import name2relation
from functools import partial
import operator
import os

_RELATIONS_DIRECTORY = os.path.abspath('app/relations')

def find_matches(triples, threshold):
    """
    Find closest matching relation to every triple in a list.

    Matches are filtered such that only matches below the threshold are accepted.

    :type triples: list of app.types.triple.Triple
    :type threshold: float
    :rtype: list of app.types.match.Match
    """
    matches = []
    relations = [name for name in os.listdir(_RELATIONS_DIRECTORY) if os.path.isdir(os.path.join(_RELATIONS_DIRECTORY, name))]
    for triple in triples:
        match = _find_triple_match(relations, triple, threshold)
        if match is not None:
            matches.append(match)
    return matches

def input2matches(user_input, threshold):
    """
    Transform preprocessed and validated user input to a list of matches.

    :type user_input: user_input
    :type threshold: float
    :rtype: list of app.types.match.Match
    """
    triples = create_triples(user_input)
    return find_matches(triples, threshold)

def _find_triple_match(relations, triple, threshold):
    best_match = None
    for relation_name in relations:
        sentences_path = os.path.join(_RELATIONS_DIRECTORY, relation_name, 'sentences.txt')
        with open(sentences_path, 'r') as sentences_file:
            sentences_content = sentences_file.read()
            lines = sentences_content.split('\n')
            sentences = list(filter(partial(operator.ne, ''), lines))
        for target_sentence in sentences:
            # TODO replace entities in sentence with triple entities
            distance = get_distance(triple.predicate_decsription, target_sentence)
            # if distance < threshold and (best_match is None or distance < best_match.distance):
            if best_match is None or distance < best_match.distance:
                relation_class = name2relation(relation_name)
                relation = relation_class(triple.entity1, triple.entity2)
                best_match = Match(relation, distance)
    return best_match
