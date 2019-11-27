"""Test that a fact, which has no similarity to any of the relations, cannot be matched."""

from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import no_match

def no_match_for_unknown_facts(threshold):
    """
    Evaluate no_match_for_unknown_facts test.

    :type threshold: float
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_no_match('an apple is a kind of fruit', threshold),
        _test_no_match('queen elizabeth ruled for several years', threshold),
        _test_no_match('george martin is a popular author', threshold),
        _test_no_match('no one have been on the sun', threshold),
        _test_no_match('sir isaac newton had an apple fall on his head', threshold),
        _test_no_match('the theory of general relativity is related to physics', threshold),
        _test_no_match('a school has many classrooms', threshold),
        _test_no_match('the spirit of the tradition lives on in many places', threshold),
        _test_no_match('no match should be found for this fact', threshold)
    ]
    return EvaluationResult('no_match_for_unknown_facts', test_results)

def _test_no_match(sentence, threshold):
    passed, fail_reason = no_match(sentence, threshold)
    return TestResult('no_fact_match', sentence, passed, fail_reason)
