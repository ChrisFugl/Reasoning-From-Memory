"""Test that a question can be answered using inference."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.utils import name2relation
from evaluation.types import EvaluationResult, TestResult
from evaluation.utils import answer_question

def answer_with_inference(threshold, matcher):
    """
    Evaluate answer_with_inference test.

    :type threshold: float
    :type matcher: app.match.Matcher
    :rtype: evaluation.types.EvaluationResult
    """
    test_results = [
        # contained_in_location is transitive
        _test_reason(threshold, matcher, 'seoul is in asia', 'contained_in_location', 'seoul', 'asia', [
            ('contained_in_location', 'seoul', 'korea'),
            ('contained_in_location', 'korea', 'asia'),
        ]),
        # a location cannot contain the location that it is contained by
        _test_reason(threshold, matcher, 'korea is not in seoul', 'contained_in_location_negated', 'korea', 'seoul', [
            ('contained_in_location', 'seoul', 'korea')
        ]),
        # a location is not in another location if the first location is in a location that is not in the second location
        _test_reason(threshold, matcher, 'copenhagen is not in asia', 'contained_in_location_negated', 'copenhagen', 'korea', [
            ('contained_in_location', 'copenhagen', 'denmark'),
            ('contained_in_location_negated', 'denmark', 'asia')
        ]),
        # children are descendants
        _test_reason(threshold, matcher, 'john descends from mia', 'descends_from', 'john', 'mia', [
            ('child_of', 'john', 'mia')
        ]),
        # descends_from is transitive
        _test_reason(threshold, matcher, 'john descends from steve', 'descends_from', 'john', 'steve', [
            ('descends_from', 'john', 'mia'),
            ('descends_from', 'mia', 'steve')
        ]),
        # an ancestor is not a descendant
        _test_reason(threshold, matcher, 'steve does not descend from john', 'descends_from_negated', 'steve', 'john', [
            ('descends_from', 'john', 'steve')
        ]),
        # person A cannot descend from person B if B has an ancesor that A does not descend from
        _test_reason(threshold, matcher, 'john does not descend from carl', 'descends_from_negated', 'john', 'carl', [
            ('descends_from_negated', 'john', 'steve'),
            ('descends_from', 'carl', 'steve')
        ]),
        # person A lives in location C when A lives in B and location B is contained in C
        _test_reason(threshold, matcher, 'john lives in denmark', 'lives_in', 'john', 'denmark', [
            ('lives_in', 'john', 'copenhagen'),
            ('contained_in_location', 'copenhagen', 'denmark')
        ]),
        # person A does not live in location B if B is contained by C and A does not live in C
        _test_reason(threshold, matcher, 'john does not live in seoul', 'lives_in_negated', 'john', 'seoul', [
            ('lives_in_negated', 'john', 'korea'),
            ('contained_in_location', 'seoul', 'korea')
        ]),
        # married is a commutative relation
        _test_reason(threshold, matcher, 'adam is married to eve', 'married_to', 'adam', 'eve', [
            ('married_to', 'eve', 'adam')
        ]),
        # marriage negated is a commutative relation
        _test_reason(threshold, matcher, 'mia is not married to john', 'married_to_negated', 'mia', 'john', [
            ('married_to_negated', 'john', 'mia')
        ]),
        # a child is related to its parent
        _test_reason(threshold, matcher, 'john is related to mia', 'related_to', 'john', 'mia', [
            ('child_of', 'john', 'mia')
        ]),
        # a parent is related to its child
        _test_reason(threshold, matcher, 'mia is related to john', 'related_to', 'mia', 'john', [
            ('child_of', 'john', 'mia')
        ]),
        # married people are related
        _test_reason(threshold, matcher, 'adam is related to eve', 'related_to', 'adam', 'eve', [
            ('married_to', 'adam', 'eve')
        ]),
        # descendants are related to their ancestors
        _test_reason(threshold, matcher, 'john is related to steve', 'related_to', 'john', 'steve', [
            ('descends_from', 'john', 'steve')
        ]),
        # ancestors are related to their descendants
        _test_reason(threshold, matcher, 'steve is related to john', 'related_to', 'steve', 'john', [
            ('descends_from', 'john', 'steve')
        ]),
        # siblings are related
        _test_reason(threshold, matcher, 'john is related to lois', 'related_to', 'john', 'lois', [
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
