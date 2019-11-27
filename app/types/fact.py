"""Contains an implementation of the fact representation."""

class Fact:
    """Representation of a fact."""

    def __init__(self, relation):
        """
        Create a fact.

        :type relation: app.types.relation.Relation
        """
        self.relation = relation

    def __eq__(self, other):
        """Equality check."""
        if other is None:
            return False
        elif not isinstance(other, Fact):
            return False
        else:
            return self.relation == other.relation

    def __ne__(self, other):
        """Inequality check."""
        if other is None:
            return True
        elif not isinstance(other, Fact):
            return True
        else:
            return self.relation != other.relation
