"""Various utility functions."""

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
