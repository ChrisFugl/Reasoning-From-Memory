"""Contains a class to temporarily memorize and reason about facts."""

from app.memories.memory import Memory

class TemporaryMemory(Memory):
    """Temporary storage of facts."""

    def __init__(self):
        """Create a temporary memory store."""
        self.facts = []

    def store(self, facts):
        """
        Memorize facts.

        :type facts: list of app.types.fact.Fact
        """
        for fact in facts:
            self.facts.append(fact)
