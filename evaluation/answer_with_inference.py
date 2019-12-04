"""Test that a question can be answered using inference."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.utils import name2relation
from functools import partial
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_with_inference(threshold, matcher):
    """
    Evaluate answer_with_inference test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    run_test = partial(_test_reason, threshold, matcher)
    test_results = [
        # contained_in_location is transitive
        run_test('Is Seoul in Asia?', 'contained_in_location', 'seoul', 'asia', [
            ('contained_in_location', 'seoul', 'korea'),
            ('contained_in_location', 'korea', 'asia'),
        ]),
        # a location cannot contain the location that it is contained by
        run_test('Is korea not in Seoul?', 'contained_in_location_negated', 'korea', 'seoul', [
            ('contained_in_location', 'seoul', 'korea')
        ]),
        # a location is not in another location if the first location is in a location that is not in the second location
        run_test('Does Copenhagen not lie in Asia?', 'contained_in_location_negated', 'copenhagen', 'korea', [
            ('contained_in_location', 'copenhagen', 'denmark'),
            ('contained_in_location_negated', 'denmark', 'asia')
        ]),
        # children are descendants
        run_test('Does John descend from Mia?', 'descends_from', 'john', 'mia', [
            ('child_of', 'john', 'mia')
        ]),
        # descends_from is transitive
        run_test('Does John descend from Steve?', 'descends_from', 'john', 'steve', [
            ('descends_from', 'john', 'mia'),
            ('descends_from', 'mia', 'steve')
        ]),
        # an ancestor is not a descendant
        run_test('Is Steve not a descendant of John?', 'descends_from_negated', 'steve', 'john', [
            ('descends_from', 'john', 'steve')
        ]),
        # person A cannot descend from person B if B has an ancesor that A does not descend from
        run_test('Does John not descend from Carl?', 'descends_from_negated', 'john', 'carl', [
            ('descends_from_negated', 'john', 'steve'),
            ('descends_from', 'carl', 'steve')
        ]),
        # person A lives in location C when A lives in B and location B is contained in C
        run_test('Is John from Denmark?', 'lives_in', 'john', 'denmark', [
            ('lives_in', 'john', 'copenhagen'),
            ('contained_in_location', 'copenhagen', 'denmark')
        ]),
        # person A does not live in location B if B is contained by C and A does not live in C
        run_test('Is John not from Seoul?', 'lives_in_negated', 'john', 'seoul', [
            ('lives_in_negated', 'john', 'korea'),
            ('contained_in_location', 'seoul', 'korea')
        ]),
        # married is a commutative relation
        run_test('Is Adam married to Eve?', 'married_to', 'adam', 'eve', [
            ('married_to', 'eve', 'adam')
        ]),
        # marriage negated is a commutative relation
        run_test('Is Mia not married to John?', 'married_to_negated', 'mia', 'john', [
            ('married_to_negated', 'john', 'mia')
        ]),
        # a child is related to its parent
        run_test('Are John and Mia related?', 'related_to', 'john', 'mia', [
            ('child_of', 'john', 'mia')
        ]),
        # a parent is related to its child
        run_test('Is Mia related to John?', 'related_to', 'mia', 'john', [
            ('child_of', 'john', 'mia')
        ]),
        # married people are related
        run_test('Is Adam related to Eve?', 'related_to', 'adam', 'eve', [
            ('married_to', 'adam', 'eve')
        ]),
        # descendants are related to their ancestors
        run_test('Is John related to Steve?', 'related_to', 'john', 'steve', [
            ('descends_from', 'john', 'steve')
        ]),
        # ancestors are related to their descendants
        run_test('Is Steve related to John?', 'related_to', 'steve', 'john', [
            ('descends_from', 'john', 'steve')
        ]),
        # siblings are related
        run_test('Is John related to Lois?', 'related_to', 'john', 'lois', [
            ('child_of', 'john', 'mia'),
            ('child_of', 'lois', 'mia')
        ]),
    ]
    return EvaluationResult('answer_with_inference', test_results)

def _test_reason(threshold, matcher, sentence, relation_name, entity1, entity2, fact_tuples):
    name = relation_name
    description = f'{relation_name}({entity1},{entity2})'
    facts = list(map(_tuple2fact, fact_tuples))
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

def _tuple2fact(fact_tuple):
    relation_name, entity1, entity2 = fact_tuple
    relation_class = name2relation(relation_name)
    relation = relation_class(entity1, entity2)
    fact = Fact(relation)
    return fact
