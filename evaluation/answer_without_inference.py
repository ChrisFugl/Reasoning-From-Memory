"""Test that a question can be answered, provided that a fact is stored for that question. Inference is not allowed."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.utils import name2relation
from functools import partial
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_without_inference(threshold, matcher):
    """
    Evaluate answer_without_inference test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    run_test = partial(_test_reason, threshold, matcher)
    test_results = [
        run_test('Is John a child of Mia?', 'child_of', 'john', 'mia'),
        run_test('Is Mia is a child of John?', 'child_of_negated', 'mia', 'john'),
        run_test('Is Seoul is Korea?', 'contained_in_location', 'seoul', 'korea'),
        run_test('Is Korea not in Seoul?', 'contained_in_location_negated', 'korea', 'seoul'),
        run_test('Does John descend from Eve?', 'descends_from', 'john', 'eve'),
        run_test('Does Eve not descend from John?', 'descends_from_negated', 'eve', 'john'),
        run_test('Does John have an apple?', 'has_object', 'john', 'apple'),
        run_test('Does Mia not have an apple?', 'has_object_negated', 'mia', 'apple'),
        run_test('Does John live in Seoul?', 'lives_in', 'john', 'seoul'),
        run_test('Does John not live in Copenhagen?', 'lives_in_negated', 'john', 'copenhagen'),
        run_test('Is Mia married to Steve?', 'married_to', 'mia', 'steve'),
        run_test('Is John not married to Mia?', 'married_to_negated', 'john', 'mia'),
        run_test('Is Steve related to Lois?', 'related_to', 'steve', 'lois'),
        run_test('Is John not related to George?', 'related_to_negated', 'john', 'george'),
    ]
    return EvaluationResult('answer_without_inference', test_results)

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
        if answer.answer == Answer.NO:
            passed = False
            fail_reason = 'Expected answer = yes, but got answer = no.'
        elif answer.answer == Answer.YES:
            passed = True
            fail_reason = None
        else:
            passed = False
            fail_reason = 'Expected answer = yes, but got answer = unknown.'
    return TestResult(name, description, passed, fail_reason)
