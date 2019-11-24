"""Definitions of all predefined relations."""

class Relation:
    """Relation of arity 2."""

    def __init__(self, entity1, entity2):
        """
        Create a relation.

        :type entity1: str
        :type entity2: str
        """
        self.entity1 = entity1
        self.entity2 = entity2

    def __eq__(self, other):
        """Equality check."""
        if other is None:
            return False
        elif not isinstance(other, self.__class__):
            return False
        else:
            return self.entity1 == other.entity1 and self.entity2 == other.entity2

    def __ne__(self, other):
        """Inequality check."""
        if other is None:
            return True
        elif not isinstance(other, self.__class__):
            return True
        else:
            return self.entity1 != other.entity1 or self.entity2 != other.entity2

    def __str__(self):
        """Prolog string representation of relation."""
        return f'{self.relation_name}({self.entity1,self.entity2}).'

class ChildOf(Relation):
    """child_of relation."""

    relation_name = 'child_of'

class ChildOfNegated(Relation):
    """child_of_negated relation."""

    relation_name = 'child_of_negated'

class ContainedInLocation(Relation):
    """contained_in_location relation."""

    relation_name = 'contained_in_location'

class ContainedInLocationNegated(Relation):
    """contained_in_location_negated relation."""

    relation_name = 'contained_in_location_negated'

class DescendsFrom(Relation):
    """descends_from relation."""

    relation_name = 'descends_from'

class DescendsFromNegated(Relation):
    """descends_from_negated relation."""

    relation_name = 'descends_from_negated'

class HasObject(Relation):
    """has_object relation."""

    relation_name = 'has_object'

class HasObjectNegated(Relation):
    """has_object_negated relation."""

    relation_name = 'has_object_negated'

class LivesIn(Relation):
    """lives_in relation."""

    relation_name = 'lives_in'

class LivesInNegated(Relation):
    """lives_in_negated relation."""

    relation_name = 'lives_in_negated'

class MarriedTo(Relation):
    """married_to relation."""

    relation_name = 'married_to'

class MarrtiedToNegated(Relation):
    """married_to_negated relation."""

    relation_name = 'married_to_negated'

class RelatedTo(Relation):
    """related_to relation."""

    relation_name = 'related_to'

class RelatedToNegated(Relation):
    """related_to_negated relation."""

    relation_name = 'related_to_negated'
