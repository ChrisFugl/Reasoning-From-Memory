"""Test that a question can be answered with no, provided that a fact is stored for that question."""

from app.types.answer import Answer
from app.types.fact import Fact
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question, name2relation

def answer_no(threshold):
    """
    Evaluate answer_no test.

    :type threshold: float
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_reason('mia is a child of john', threshold, 'child_of', 'john', 'mia'),
        _test_reason('korea is in seoul', threshold, 'contained_in_location', 'seoul', 'korea'),
        _test_reason('eve descends from john', threshold, 'descends_from', 'john', 'eve'),
    ]
    return EvaluationResult('answer_no', test_results)

def _test_reason(sentence, threshold, relation_name, entity1, entity2):
    name = relation_name
    description = f'{relation_name}({entity1},{entity2})'
    relation_class = name2relation(relation_name)
    relation = relation_class(entity1, entity2)
    facts = [Fact(relation)]
    answer, passed_answer, fail_reason_answer = answer_question(sentence, threshold, relation_name, entity1, entity2, facts)
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
