"""Test that a fact, which has no similarity to any of the relations, cannot be matched."""

from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import no_match

def no_match_for_unknown_facts(threshold, matcher):
    """
    Evaluate no_match_for_unknown_facts test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_no_match(threshold, matcher, 'an apple is a kind of fruit'),
        _test_no_match(threshold, matcher, 'queen elizabeth ruled for several years'),
        _test_no_match(threshold, matcher, 'george martin is a popular author'),
        _test_no_match(threshold, matcher, 'no one have been on the sun'),
        _test_no_match(threshold, matcher, 'sir isaac newton had an apple fall on his head'),
        _test_no_match(threshold, matcher, 'the theory of general relativity is related to physics'),
        _test_no_match(threshold, matcher, 'a school has many classrooms'),
        _test_no_match(threshold, matcher, 'the spirit of the tradition lives on in many places'),
        _test_no_match(threshold, matcher, 'no match should be found for this fact')
    ]
    return EvaluationResult('no_match_for_unknown_facts', test_results)

def _test_no_match(threshold, matcher, sentence):
    passed, fail_reason = no_match(threshold, matcher, sentence)
    return TestResult('no_fact_match', sentence, passed, fail_reason)
