"""Contains definition of a relation match."""

class Match:
    """Definition of a relation match."""

    def __init__(self, relation, distance):
        """
        Create a match.

        :type relation: app.types.relation.Relation
        :type distance: float
        """
        self.relation = relation
        self.distance = distance
