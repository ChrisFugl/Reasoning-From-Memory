"""Contains function to match a triples with relations."""

from app.distance import distance as get_distance
from app.triple import create_triples
from app.types.match import Match
from app.utils import name2relation
from functools import partial
import operator
import os

_RELATIONS_DIRECTORY = os.path.abspath('app/relations')

class Matcher:
    """Match user input with predefined relations."""

    def __init__(self, nlp):
        """
        Create a matcher.

        :type threshold: float
        :type nlp: spacy.language.Language
        """
        self.nlp = nlp
        self.relations = self._get_relations()

    def input2matches(self, threshold, user_input):
        """
        Transform preprocessed and validated user input to a list of matches.

        :type threshold: float
        :type user_input: user_input
        :rtype: list of app.types.match.Match
        """
        triples = create_triples(self.nlp, user_input)
        return self._find_matches(threshold, triples)

    def _find_matches(self, threshold, triples):
        """
        Find closest matching relation to every triple in a list.

        Matches are filtered such that only matches below the threshold are accepted.

        :type threshold: float
        :type triples: list of app.types.triple.Triple
        :rtype: list of app.types.match.Match
        """
        matches = []
        for triple in triples:
            match = self._find_triple_match(threshold, triple)
            if match is not None:
                matches.append(match)
        return matches

    def _find_triple_match(self, threshold, triple):
        best_match = None
        for relation_name, sentences in self.relations:
            insert_entities_in_sentence = partial(self._insert_entities_in_sentence, triple)
            target_sentences = map(insert_entities_in_sentence, sentences)
            for target_sentence in target_sentences:
                distance = get_distance(self.nlp, triple.predicate_decsription, target_sentence)
                if distance < threshold and (best_match is None or distance < best_match.distance):
                    relation_class = name2relation(relation_name)
                    relation = relation_class(triple.entity1, triple.entity2)
                    best_match = Match(relation, distance)
        return best_match

    def _insert_entities_in_sentence(self, triple, sentence):
        sentence_with_entity1 = sentence.replace('[1]', triple.entity1)
        sentence_with_entities = sentence_with_entity1.replace('[2]', triple.entity2)
        return sentence_with_entities

    def _get_relations(self):
        relations = []
        relations_directory_contents = os.listdir(_RELATIONS_DIRECTORY)
        for relation_name in relations_directory_contents:
            relation_directory = os.path.join(_RELATIONS_DIRECTORY, relation_name)
            if os.path.isdir(relation_directory):
                relation_sentences = self._get_relation_sentences(relation_name)
                relation = (relation_name, relation_sentences)
                relations.append(relation)
        return relations

    def _get_relation_sentences(self, relation_name):
        sentences_path = os.path.join(_RELATIONS_DIRECTORY, relation_name, 'sentences.txt')
        with open(sentences_path, 'r') as sentences_file:
            sentences_content = sentences_file.read()
            lines = sentences_content.split('\n')
            sentences = list(filter(partial(operator.ne, ''), lines))
        return sentences
