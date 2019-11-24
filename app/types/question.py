"""Contains an representation of a question."""

class Question:
    """Representation of question."""

    def __init__(self, relation):
        """
        Create a question.

        :type relation: app.types.relation.Relation
        """
        self.relation = relation
