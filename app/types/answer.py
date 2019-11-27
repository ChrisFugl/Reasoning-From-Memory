"""Contains a representation of an answer to a question."""

class Answer:
    """Representation of an answer."""

    UNKNOWN = 1
    NO = 2
    YES = 3

    def __init__(self, answer, question):
        """
        Create an answer.

        :type answer: int
        :type question: app.types.question.Question
        """
        assert answer in [Answer.UNKNOWN, Answer.NO, Answer.YES]
        self.answer = answer
        self.question = question

    def __eq__(self, other):
        """Equality check."""
        if other is None:
            return False
        elif not isinstance(other, Answer):
            return False
        else:
            return self.answer == other.answer and self.question == other.question

    def __ne__(self, other):
        """Inequality check."""
        if other is None:
            return True
        elif not isinstance(other, Answer):
            return True
        else:
            return self.answer != other.answer or self.question != other.question
