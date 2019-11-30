"""Test that a question, which has no similarity to any of the relations, cannot be matched."""

from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import no_match

def no_match_for_unknown_questions(threshold, matcher):
    """
    Evaluate no_match_for_unknown_questions test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_no_match(threshold, matcher, 'is an apple a kind of fruit'),
        _test_no_match(threshold, matcher, 'did queen elizabeth rule for several years'),
        _test_no_match(threshold, matcher, 'is george martin a popular author'),
        _test_no_match(threshold, matcher, 'has anyone ever been on the sun'),
        _test_no_match(threshold, matcher, 'did sir isaac newton have an apple fall on his head'),
        _test_no_match(threshold, matcher, 'is the theory of general relativity related to physics'),
        _test_no_match(threshold, matcher, 'does a school have many classrooms'),
        _test_no_match(threshold, matcher, 'does the spirit of the tradition live on in many places'),
        _test_no_match(threshold, matcher, 'is it true that no match should be found for this question'),
        _test_no_match(threshold, matcher, 'is there a relation between physics and chemistry'),
        _test_no_match(threshold, matcher, 'are athena and poseidon the children of zeus'),
        _test_no_match(threshold, matcher, 'is the penguin a natural inhabitant of the saharan desert')
    ]
    return EvaluationResult('no_match_for_unknown_questions', test_results)

def _test_no_match(threshold, matcher, sentence):
    passed, fail_reason = no_match(threshold, matcher, sentence)
    return TestResult('no_fact_match', sentence, passed, fail_reason)
