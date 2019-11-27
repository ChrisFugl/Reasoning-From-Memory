"""Test that a sentence in a relation can be matched to itself."""

from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import match_with_relation_name
from functools import partial
import operator
import os

_RELATIONS_DIRECTORY = os.path.abspath('app/relations')

def match_to_self(threshold):
    """
    Evaluate match_to_self test.

    :type threshold: float
    :rtype: evaluation.types.EvaluationResult
    """
    relations = os.listdir(_RELATIONS_DIRECTORY)
    test_results = []
    for relation in relations:
        relation_absolute_path = os.path.join(_RELATIONS_DIRECTORY, relation)
        if os.path.isdir(relation_absolute_path):
            results = _test_relation(relation, threshold)
            for result in results:
                test_results.append(result)
    return EvaluationResult('match_to_self', test_results)

def _test_relation(relation_name, threhold):
    sentences_path = os.path.join(_RELATIONS_DIRECTORY, relation_name, 'sentences.txt')
    with open(sentences_path, 'r') as sentences_file:
        sentences_content = sentences_file.read()
        lines = sentences_content.split('\n')
        sentences = list(filter(partial(operator.ne, ''), lines))
        test = partial(_test_relation_sentence, relation_name, threhold)
    test_results = list(map(test, sentences))
    return test_results

def _test_relation_sentence(relation_name, threshold, sentence):
    _, passed, fail_reason = match_with_relation_name(sentence, threshold, relation_name)
    return TestResult(relation_name, sentence, passed, fail_reason)
