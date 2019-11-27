"""Contains an representation of a question."""

class Question:
    """Representation of question."""

    def __init__(self, relation):
        """
        Create a question.

        :type relation: app.types.relation.Relation
        """
        self.relation = relation

    def __eq__(self, other):
        """Equality check."""
        if other is None:
            return False
        elif not isinstance(other, Question):
            return False
        else:
            return self.relation == other.relation

    def __ne__(self, other):
        """Inequality check."""
        if other is None:
            return True
        elif not isinstance(other, Question):
            return True
        else:
            return self.relation != other.relation
