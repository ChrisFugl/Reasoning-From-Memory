"""Contains a class to temporarily memorize and reason about facts."""

class TemporaryMemory:
    """Temporary storage of facts."""

    def store(self, facts):
        """
        Memorize facts.

        :type facts: list of app.types.fact.Fact
        """
        raise NotImplementedError()
