"""Contains definition of a triple."""

class Triple:
    """Definition of a triple."""

    def __init__(self, predicate_decsription, entity1, entity2):
        """
        Create a match.

        :type predicate_decsription: str
        :type entity1: str
        :type entity2: str
        """
        self.predicate_decsription = predicate_decsription
        self.entity1 = entity1
        self.entity2 = entity2

    def __str__(self):
        """Represent triple as a string."""
        return f'({self.predicate_decsription}, {self.entity1}, {self.entity2})'
