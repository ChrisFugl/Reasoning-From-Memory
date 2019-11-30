"""Contains function to match a triples with relations."""

from app.distance import distance as get_distance
from app.triple import create_triples
from app.types.match import Match
from app.utils import name2relation
from functools import partial
import operator
import os

_RELATIONS_DIRECTORY = os.path.abspath('app/relations')

def input2matches(threshold, nlp, user_input):
    """
    Transform preprocessed and validated user input to a list of matches.

    :type threshold: float
    :type nlp: spacy.language.Language
    :type user_input: user_input
    :rtype: list of app.types.match.Match
    """
    triples = create_triples(nlp, user_input)
    return find_matches(threshold, nlp, triples)

def find_matches(threshold, nlp, triples):
    """
    Find closest matching relation to every triple in a list.

    Matches are filtered such that only matches below the threshold are accepted.

    :type threshold: float
    :type nlp: spacy.language.Language
    :type triples: list of app.types.triple.Triple
    :rtype: list of app.types.match.Match
    """
    matches = []
    relations = [name for name in os.listdir(_RELATIONS_DIRECTORY) if os.path.isdir(os.path.join(_RELATIONS_DIRECTORY, name))]
    for triple in triples:
        match = _find_triple_match(threshold, nlp, relations, triple)
        if match is not None:
            matches.append(match)
    return matches

def _find_triple_match(threshold, nlp, relations, triple):
    best_match = None
    for relation_name in relations:
        sentences_path = os.path.join(_RELATIONS_DIRECTORY, relation_name, 'sentences.txt')
        with open(sentences_path, 'r') as sentences_file:
            sentences_content = sentences_file.read()
            lines = sentences_content.split('\n')
            sentences = list(filter(partial(operator.ne, ''), lines))
        insert_entities_in_sentence = partial(_insert_entities_in_sentence, triple)
        target_sentences = map(insert_entities_in_sentence, sentences)
        for target_sentence in target_sentences:
            distance = get_distance(nlp, triple.predicate_decsription, target_sentence)
            # if distance < threshold and (best_match is None or distance < best_match.distance):
            if best_match is None or distance < best_match.distance:
                relation_class = name2relation(relation_name)
                relation = relation_class(triple.entity1, triple.entity2)
                best_match = Match(relation, distance)
    return best_match

def _insert_entities_in_sentence(triple, sentence):
    sentence_with_entity1 = sentence.replace('[1]', triple.entity1)
    sentence_with_entities = sentence_with_entity1.replace('[2]', triple.entity2)
    return sentence_with_entities
