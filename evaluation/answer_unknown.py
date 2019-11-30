"""Test that a question will be answered as unknown when the fact is not stored and cannot be inferred."""

from app.types.answer import Answer
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_unknown(threshold, matcher):
    """
    Evaluate answer_unknown test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_reason(threshold, matcher, 'john is a child of mia', 'child_of', 'john', 'mia'),
        _test_reason(threshold, matcher, 'mia is not a child of john', 'child_of_negated', 'mia', 'john'),
        _test_reason(threshold, matcher, 'seoul is in korea', 'contained_in_location', 'seoul', 'korea'),
        _test_reason(threshold, matcher, 'korea is not in seoul', 'contained_in_location_negated', 'korea', 'seoul'),
        _test_reason(threshold, matcher, 'john descends from eve', 'descends_from', 'john', 'eve'),
        _test_reason(threshold, matcher, 'eve does not descend from john', 'descends_from_negated', 'eve', 'john'),
        _test_reason(threshold, matcher, 'john has an apple', 'has_object', 'john', 'apple'),
        _test_reason(threshold, matcher, 'mia does not have an apple', 'has_object_negated', 'mia', 'apple'),
        _test_reason(threshold, matcher, 'john lives in seoul', 'lives_in', 'john', 'seoul'),
        _test_reason(threshold, matcher, 'john does not live in copenhagen', 'lives_in_negated', 'john', 'copenhagen'),
        _test_reason(threshold, matcher, 'mia is married to steve', 'married_to', 'mia', 'steve'),
        _test_reason(threshold, matcher, 'john is not married to mia', 'married_to_negated', 'john', 'mia'),
        _test_reason(threshold, matcher, 'steve is related to lois', 'related_to', 'steve', 'lois'),
        _test_reason(threshold, matcher, 'john is not related to george', 'related_to_negated', 'john', 'george'),
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
