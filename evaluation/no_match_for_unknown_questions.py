"""Test that a question, which has no similarity to any of the relations, cannot be matched."""

from functools import partial
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import no_match

def no_match_for_unknown_questions(threshold, matcher):
    """
    Evaluate no_match_for_unknown_questions test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    run_test = partial(_test_no_match, threshold, matcher)
    test_results = [
        run_test('Is an apple a kind of fruit?'),
        run_test('Did Queen Elizabeth rule for several years?'),
        run_test('Is George Martin a popular author?'),
        run_test('Have anyone ever been on the sun?'),
        run_test('Did Sir Isaac Newton have an apple fall on his head?'),
        run_test('Is the theory of general relativity related to physics?'),
        run_test('Does a school have many classrooms?'),
        run_test('Does the spirit of the tradition live on in many places?'),
        run_test('Is it true that no match should be found for this question?'),
        run_test('Is there a relation between physics and chemistry?'),
        run_test('Are Athena and Ares the children of Zeus?'),
        run_test('Is the penguin not a natural inhabitant of the Sahara desert?'),
        run_test('Are there over 7 billion people on Earth?'),
        run_test('Did he kill two birds with one stone?'),
        run_test('Is Canada known for maple syrup?'),
        run_test('Will the Olympic games take place in Tokyo in 2020?'),
        run_test('Does Genghis Khan have many descendants?'),
        run_test('Does a house usually have more than one window?'),
        run_test('Does the box not contain dangerous chemicals?'),
    ]
    return EvaluationResult('no_match_for_unknown_questions', test_results)

def _test_no_match(threshold, matcher, sentence):
    passed, fail_reason = no_match(threshold, matcher, sentence)
    return TestResult('no_fact_match', sentence, passed, fail_reason)
