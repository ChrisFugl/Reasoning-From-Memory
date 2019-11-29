"""Contains a class to persistently memorize and reason about facts."""

from app.memories.memory import Memory
from app.types.fact import Fact
from app.utils import name2relation
from functools import partial
import operator
import re

class PersistentMemory(Memory):
    """Persistent storage of facts."""

    def __init__(self, memory_path):
        """Create a persistent memory."""
        self.memory_path = memory_path
        self.facts = self._load()

    def store(self, new_facts):
        """
        Memorize facts.

        :type facts: list of app.types.fact.Fact
        """
        for fact in new_facts:
            if not fact in self.facts:
                self.facts.append(fact)
        self._write_facts(self.facts)

    def _load(self):
        try:
            with open(self.memory_path, 'r') as memory_file:
                memory_content = memory_file.read()
                lines = memory_content.split('\n')
                memories = list(filter(partial(operator.ne, ''), lines))
        except FileNotFoundError:
            memories = []
        facts = list(map(self._memory2fact, memories))
        return facts

    def _memory2fact(self, memory):
        pattern = r'([a-zA-Z0_]+)\(([a-zA-Z_]+),([a-zA-Z_]+)\)'
        match = re.match(pattern, memory)
        if match is None:
            raise Exception(f'Invalid fact in memory file: {memory}')
        groups = match.groups()
        relation_name = groups[0]
        entity1 = groups[1]
        entity2 = groups[2]
        relation_class = name2relation(relation_name)
        relation = relation_class(entity1, entity2)
        fact = Fact(relation)
        return fact

    def _write_facts(self, facts):
        memory_content = '\n'.join(map(str, facts))
        with open(self.memory_path, 'w') as memory_file:
            memory_file.write(memory_content)
