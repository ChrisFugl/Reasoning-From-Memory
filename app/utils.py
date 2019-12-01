"""Various utility functions."""

from app.types.answer import Answer
from app.types.fact import Fact
from app.types.question import Question
import app.types.relation as relations

def answer2message(answer):
    """
    Convert an answer to a text reply.

    :type: app.types.answer.Answer
    :rtype: str
    """
    if answer.answer == Answer.UNKNOWN:
        return 'I do not know.'
    elif answer.answer == Answer.NO:
        return 'No.'
    else:
        return 'Yes.'

def match2fact(match):
    """
    Convert fact to a fact.

    :type: app.types.match.Match
    :rtype: app.types.fact.Fact
    """
    return Fact(match.relation)

def match2question(match):
    """
    Convert match to a question.

    :type: app.types.match.Match
    :rtype: app.types.question.Question
    """
    return Question(match.relation)

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
