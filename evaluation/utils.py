"""Contains methods to help evaluate the application."""

from app.match import input2matches
from app.reason import reason
from app.types.answer import Answer
from app.types.question import Question
import app.types.relation as relations

_relation_class_map = {
    'child_of': relations.ChildOf,
    'child_of_negated': relations.ChildOfNegated,
    'contained_in_location': relations.ContainedInLocation,
    'contained_in_location_negated': relations.ContainedInLocationNegated,
    'descends_from': relations.DescendsFrom,
    'descends_from_negated': relations.DescendsFromNegated,
    'has_object': relations.HasObject,
    'has_object_negated': relations.HasObjectNegated,
    'lives_in': relations.LivesIn,
    'lives_in_negated': relations.LivesInNegated,
    'married_to': relations.MarriedTo,
    'married_to_negated': relations.MarrtiedToNegated,
    'related_to': relations.RelatedTo,
    'related_to_negated': relations.RelatedToNegated,
}

def name2relation(relation_name):
    """Get a relation class from a relation name."""
    return _relation_class_map[relation_name]

def match_with_relation(sentence, threshold, relation_name, entity1, entity2):
    """
    Check if a match can be found for a sentence.

    :type sentence: str
    :type threshold: float
    :type relation_name: str
    :type entity1: str
    :type entity2: str
    :rtype: (app.types.match.Match, bool, str)
    """
    relation_class = name2relation(relation_name)
    expected_relation = relation_class(entity1, entity2)
    try:
        matches = input2matches(sentence, threshold)
        if len(matches) == 0:
            match = None
            passed = False
            fail_reason = f'No matches found (threshold = {threshold}).'
        else:
            found_match = False
            for match in matches:
                found_match = found_match or match.relation == expected_relation
                if found_match:
                    break
            if found_match:
                passed = True
                fail_reason = None
            else:
                match = None
                passed = False
                fail_reason = f'Did not match with {expected_relation}.'
    except:
        match = None
        passed = False
        fail_reason = 'Exception.'
    return match, passed, fail_reason

def match_with_relation_name(sentence, threshold, relation_name):
    """
    Check if a match can be found for a sentence.

    :type sentence: str
    :type threshold: float
    :type relation_name: str
    :rtype: (app.types.match.Match, bool, str)
    """
    try:
        matches = input2matches(sentence, threshold)
        if len(matches) == 0:
            match = None
            passed = False
            fail_reason = f'No matches found (threshold = {threshold}).'
        else:
            found_match = False
            for match in matches:
                found_match = found_match or match.relation.relation_name == relation_name
                if found_match:
                    break
            if found_match:
                passed = True
                fail_reason = None
            else:
                match = None
                passed = False
                fail_reason = f'Did not match with {relation_name}.'
    except:
        match = None
        passed = False
        fail_reason = 'Exception.'
    return match, passed, fail_reason

def no_match(sentence, threshold):
    """
    Check that a match cannot be found for a sentence.

    :type sentence: str
    :type threshold: float
    :rtype: (bool, str)
    """
    try:
        matches = input2matches(sentence, threshold)
        if len(matches) == 0:
            passed = True
            fail_reason = None
        else:
            passed = False
            fail_reason = 'Found a match.'
    except:
        passed = False
        fail_reason = 'Exception.'
    return passed, fail_reason

def answer_question(sentence, threshold, relation_name, entity1, entity2, facts):
    """
    Attempt to answer a question.

    :type sentence: str
    :type threshold: float
    :type facts: list of app.types.fact.Fact
    :type relation_name: str
    :type entity1: str
    :type entity2: str
    :rtype: (app.types.answer.Answer, bool, str)
    """
    match, passed_match, match_fail_reason = match_with_relation(sentence, threshold, relation_name, entity1, entity2)
    if not passed_match:
        return None, False, match_fail_reason
    else:
        question = Question(match.relation)
        try:
            answer = reason(facts, question)
            return answer, True, None
        except:
            return None, False, 'Exception.'
