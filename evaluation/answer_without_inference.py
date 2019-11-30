"""Test that a question can be answered, provided that a fact is stored for that question. Inference is not allowed."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.utils import name2relation
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_without_inference(threshold, matcher):
    """
    Evaluate answer_without_inference test.

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
