"""Test that a question will be answered as unknown when the fact is not stored and cannot be inferred."""

from app.types.answer import Answer
from functools import partial
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_unknown(threshold, matcher):
    """
    Evaluate answer_unknown test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    run_test = partial(_test_reason, threshold, matcher)
    test_results = [
        run_test('Is John a child of Mia?', 'child_of', 'john', 'mia'),
        run_test('Is Mia not John\'s daughter?', 'child_of_negated', 'mia', 'john'),
        run_test('Does Seoul lie in Korea?', 'contained_in_location', 'seoul', 'korea'),
        run_test('Does Korea not lie in Seoul?', 'contained_in_location_negated', 'korea', 'seoul'),
        run_test('Is John a descendant of Eve?', 'descends_from', 'john', 'eve'),
        run_test('Is Eve not a descendant of John?', 'descends_from_negated', 'eve', 'john'),
        run_test('Does John have an apple?', 'has_object', 'john', 'apple'),
        run_test('Does Mia not have an apple?', 'has_object_negated', 'mia', 'apple'),
        run_test('Is John from Seoul?', 'lives_in', 'john', 'seoul'),
        run_test('Does John not live in Copenhagen?', 'lives_in_negated', 'john', 'copenhagen'),
        run_test('Is Mia married to Steve?', 'married_to', 'mia', 'steve'),
        run_test('Are John not married to Mia?', 'married_to_negated', 'john', 'mia'),
        run_test('Is Steve related to Lois?', 'related_to', 'steve', 'lois'),
        run_test('Is John not related to George?', 'related_to_negated', 'john', 'george'),
    ]
    return EvaluationResult('answer_unknown', test_results)

def _test_reason(threshold, matcher, sentence, relation_name, entity1, entity2):
    name = relation_name
    description = f'{relation_name}({entity1},{entity2})'
    facts = []
    answer, passed_answer, fail_reason_answer = answer_question(threshold, matcher, sentence, relation_name, entity1, entity2, facts)
    if not passed_answer:
        passed = False
        fail_reason = fail_reason_answer
    else:
        if answer.answer == Answer.NO:
            passed = False
            fail_reason = 'Expected answer = unknown, but got answer = no.'
        elif answer.answer == Answer.YES:
            passed = False
            fail_reason = 'Expected answer = unknown, but got answer = yes.'
        else:
            passed = True
            fail_reason = None
    return TestResult(name, description, passed, fail_reason)
