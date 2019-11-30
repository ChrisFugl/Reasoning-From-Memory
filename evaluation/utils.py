"""Contains methods to help evaluate the application."""

from app.match import input2matches
from app.reason import reason
from app.types.question import Question
from app.utils import name2relation

def match_with_relation(threshold, nlp, sentence, relation_name, entity1, entity2):
    """
    Check if a match can be found for a sentence.

    :type threshold: float
    :type nlp: spcacy.language.Language
    :type sentence: str
    :type relation_name: str
    :type entity1: str
    :type entity2: str
    :rtype: (app.types.match.Match, bool, str)
    """
    relation_class = name2relation(relation_name)
    expected_relation = relation_class(entity1, entity2)
    try:
        matches = input2matches(threshold, sentence, nlp)
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

def match_with_relation_name(threshold, nlp, sentence, relation_name):
    """
    Check if a match can be found for a sentence.

    :type threshold: float
    :type nlp: spcacy.language.Language
    :type sentence: str
    :type relation_name: str
    :rtype: (app.types.match.Match, bool, str)
    """
    try:
        matches = input2matches(threshold, nlp, sentence)
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

def no_match(threshold, nlp, sentence):
    """
    Check that a match cannot be found for a sentence.

    :type threshold: float
    :type nlp: spcacy.language.Language
    :type sentence: str
    :rtype: (bool, str)
    """
    try:
        matches = input2matches(threshold, nlp, sentence)
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

def answer_question(threshold, nlp, sentence, relation_name, entity1, entity2, facts):
    """
    Attempt to answer a question.

    :type threshold: float
    :type nlp: spcacy.language.Language
    :type sentence: str
    :type facts: list of app.types.fact.Fact
    :type relation_name: str
    :type entity1: str
    :type entity2: str
    :rtype: (app.types.answer.Answer, bool, str)
    """
    match, passed_match, match_fail_reason = match_with_relation(threshold, nlp, sentence, relation_name, entity1, entity2)
    if not passed_match:
        return None, False, match_fail_reason
    else:
        question = Question(match.relation)
        try:
            answer = reason(facts, question)
            return answer, True, None
        except:
            return None, False, 'Exception.'
