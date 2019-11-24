"""Contains a base class to memorize and reason about facts."""

class Memory:
    """Base class to store facts."""

    def __init__(self, facts=[]):
        """
        Create a memory store.

        :type facts: list of app.types.fact.Fact
        """
        self.facts = facts

    def store(self, facts):
        """
        Memorize facts.

        :type facts: list of app.types.fact.Fact
        """
        raise NotImplementedError('store should be implemented by subclass')
