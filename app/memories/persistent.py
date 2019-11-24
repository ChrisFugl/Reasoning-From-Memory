"""Contains a class to persistently memorize and reason about facts."""

from app.memories.memory import Memory

class PersistentMemory(Memory):
    """Persistent storage of facts."""

    def __init__(self):
        """Create a persistent memory."""
        # TODO: load facts from file
        self.facts = []

    def store(self, facts):
        """
        Memorize facts.

        :type facts: list of app.types.fact.Fact
        """
        raise NotImplementedError()
