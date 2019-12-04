"""Test that a question can be answered with no, provided that a fact is stored for that question."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.utils import name2relation
from functools import partial
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_no(threshold, matcher):
    """
    Evaluate answer_no test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    run_test = partial(_test_reason, threshold, matcher)
    test_results = [
        run_test('Is Mia a child of John?', 'child_of', 'john', 'mia'),
        run_test('Is Mia a daughter of John\'s?', 'child_of_negated', 'mia', 'john'),
        run_test('Is Korea in Seoul?', 'contained_in_location', 'seoul', 'korea'),
        run_test('Is Denmark placed in Copenhagen?', 'contained_in_location', 'denmark', 'copenhagen'),
        run_test('Does Copenhagen lie in Korea?', 'contained_in_location_negated', 'copenhagen', 'korea'),
        run_test('Does Eve descend from John?', 'descends_from', 'john', 'eve'),
        run_test('Does John come from Olivia\'s line?', 'descends_from_negated', 'john', 'olivia'),
        run_test('Does John own a Ferrari?', 'has_object_negated', 'john', 'ferrari'),
        run_test('Is John from Seoul?', 'lives_in_negated', 'john', 'seoul'),
        run_test('Are Steve and Mia married?', 'married_to_negated', 'steve', 'mia'),
        run_test('Is Mia married to Steve?', 'married_to_negated', 'steve', 'mia'),
        run_test('Are John and Carl related?', 'related_to_negated', 'john', 'carl'),
    ]
    return EvaluationResult('answer_no', test_results)

def _test_reason(threshold, matcher, sentence, relation_name, entity1, entity2):
    name = relation_name
    description = f'{relation_name}({entity1},{entity2})'
    relation_class = name2relation(relation_name)
    relation = relation_class(entity1, entity2)
    facts = [Fact(relation)]
    answer, passed_answer, fail_reason_answer = answer_question(threshold, matcher, sentence, relation_name, entity1, entity2, facts)
    if not passed_answer:
        passed = False
        fail_reason = fail_reason_answer
    else:
        if answer.answer == Answer.UNKNOWN:
            passed = False
            fail_reason = 'Expected answer = no, but got answer = unknown.'
        elif answer.answer == Answer.YES:
            passed = False
            fail_reason = 'Expected answer = no, but got answer = unknown.'
        else:
            passed = True
            fail_reason = None
    return TestResult(name, description, passed, fail_reason)
