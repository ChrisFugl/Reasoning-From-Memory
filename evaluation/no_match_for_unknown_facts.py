"""Test that a fact, which has no similarity to any of the relations, cannot be matched."""

from functools import partial
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import no_match

def no_match_for_unknown_facts(threshold, matcher):
    """
    Evaluate no_match_for_unknown_facts test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    run_test = partial(_test_no_match, threshold, matcher)
    test_results = [
        run_test('An apple is a kind of fruit.'),
        run_test('Queen Elizabeth ruled for several years.'),
        run_test('George Martin is a popular author.'),
        run_test('No one have ever been on the sun.'),
        run_test('Sir Isaac Newton had an apple fall on his head.'),
        run_test('The theory of general relativity is related to physics.'),
        run_test('A school has many classrooms.'),
        run_test('The spirit of the tradition lives on in many places.'),
        run_test('No match should be found for this fact.'),
        run_test('There is a relation between physics and chemistry.'),
        run_test('Athena and Ares are the children of Zeus.'),
        run_test('The penguin is not a natural inhabitant of the Sahara desert.'),
        run_test('There are over 7 billion people on Earth.'),
        run_test('He killed two birds with one stone.'),
        run_test('Canada is known for maple syrup.'),
        run_test('The Olympic games will take place in Tokyo in 2020.'),
        run_test('Genghis Khan has many descendants.'),
        run_test('A house usually has more than one window.'),
        run_test('The box does not contain dangerous chemicals.'),
    ]
    return EvaluationResult('no_match_for_unknown_facts', test_results)

def _test_no_match(threshold, matcher, sentence):
    passed, fail_reason = no_match(threshold, matcher, sentence)
    return TestResult('no_fact_match', sentence, passed, fail_reason)
