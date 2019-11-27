"""Test that a question, which has no similarity to any of the relations, cannot be matched."""

from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import no_match

def no_match_for_unknown_questions(threshold):
    """
    Evaluate no_match_for_unknown_questions test.

    :type threshold: float
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_no_match('is an apple a kind of fruit', threshold),
        _test_no_match('did queen elizabeth rule for several years', threshold),
        _test_no_match('is george martin a popular author', threshold),
        _test_no_match('has anyone ever been on the sun', threshold),
        _test_no_match('did sir isaac newton have an apple fall on his head', threshold),
        _test_no_match('is the theory of general relativity related to physics', threshold),
        _test_no_match('does a school have many classrooms', threshold),
        _test_no_match('does the spirit of the tradition live on in many places', threshold),
        _test_no_match('is it true that no match should be found for this question', threshold),
        _test_no_match('is there a relation between physics and chemistry', threshold),
        _test_no_match('are athena and poseidon the children of zeus', threshold),
        _test_no_match('is the penguin a natural inhabitant of the saharan desert', threshold)
    ]
    return EvaluationResult('no_match_for_unknown_questions', test_results)

def _test_no_match(sentence, threshold):
    passed, fail_reason = no_match(sentence, threshold)
    return TestResult('no_fact_match', sentence, passed, fail_reason)
