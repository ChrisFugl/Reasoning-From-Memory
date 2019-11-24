"""Contains a representation of an answer to a question."""

class Answer:
    """Representation of an answer."""

    UNKNOWN = 1
    NO = 2
    YES = 3

    def __init__(self, answer, match):
        """
        Create an answer.

        :type answer: int
        :type match: app.types.relation.Relation
        """
        assert answer in [Answer.UNKNOWN, Answer.NO, Answer.YES]
        self.answer = answer
        self.match = match

    def __eq__(self, other):
        """Equality check."""
        if other is None:
            return False
        elif not isinstance(other, Answer):
            return False
        else:
            return self.answer == other.answer and self.match == other.match

    def __ne__(self, other):
        """Inequality check."""
        if other is None:
            return True
        elif not isinstance(other, Answer):
            return True
        else:
            return self.answer != other.answer or self.match != other.match
