"""Test that a question can be answered with no, provided that a fact is stored for that question."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.utils import name2relation
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_no(threshold, matcher):
    """
    Evaluate answer_no test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        _test_reason(threshold, matcher, 'Mia is a child of John', 'child_of', 'john', 'mia'),
        _test_reason(threshold, matcher, 'Korea is in Seoul', 'contained_in_location', 'seoul', 'korea'),
        _test_reason(threshold, matcher, 'Eve descends from John', 'descends_from', 'john', 'eve'),
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
