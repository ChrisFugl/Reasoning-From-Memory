"""Test that a question will be answered as unknown when the fact is not stored and cannot be inferred."""

from app.types.answer import Answer
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_unknown(threshold):
    """
    Evaluate answer_unknown test.

    :type threshold: float
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_reason('john is a child of mia', threshold, 'child_of', 'john', 'mia'),
        _test_reason('mia is not a child of john', threshold, 'child_of_negated', 'mia', 'john'),
        _test_reason('seoul is in korea', threshold, 'contained_in_location', 'seoul', 'korea'),
        _test_reason('korea is not in seoul', threshold, 'contained_in_location_negated', 'korea', 'seoul'),
        _test_reason('john descends from eve', threshold, 'descends_from', 'john', 'eve'),
        _test_reason('eve does not descend from john', threshold, 'descends_from_negated', 'eve', 'john'),
        _test_reason('john has an apple', threshold, 'has_object', 'john', 'apple'),
        _test_reason('mia does not have an apple', threshold, 'has_object_negated', 'mia', 'apple'),
        _test_reason('john lives in seoul', threshold, 'lives_in', 'john', 'seoul'),
        _test_reason('john does not live in copenhagen', threshold, 'lives_in_negated', 'john', 'copenhagen'),
        _test_reason('mia is married to steve', threshold, 'married_to', 'mia', 'steve'),
        _test_reason('john is not married to mia', threshold, 'married_to_negated', 'john', 'mia'),
        _test_reason('steve is related to lois', threshold, 'related_to', 'steve', 'lois'),
        _test_reason('john is not related to george', threshold, 'related_to_negated', 'john', 'george'),
    ]
    return EvaluationResult('answer_unknown', test_results)

def _test_reason(sentence, threshold, relation_name, entity1, entity2):
    name = relation_name
    description = f'{relation_name}({entity1},{entity2})'
    facts = []
    answer, passed_answer, fail_reason_answer = answer_question(sentence, threshold, relation_name, entity1, entity2, facts)
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
