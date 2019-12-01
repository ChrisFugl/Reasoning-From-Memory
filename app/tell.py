"""Methods related to processing a fact."""

from app.parse import parse
from app.utils import match2fact

def tell(threshold, matcher, memory, message):
    """
    Tell a fact.

    :type threshold: float
    :type matcher: app.match.Matcher
    :type memory: app.memories.memory.Memory
    :type message: str
    """
    user_input, valid, validation_mesage = parse(message)
    if not valid:
        return {'type': 'invalid', 'message': validation_mesage}
    matches = matcher.input2matches(threshold, user_input)
    n_matches = len(matches)
    if n_matches == 0:
        # TODO: how to give detailed information on why no match was found?
        return {'type': 'no_match'}
    else:
        facts = list(map(match2fact, matches))
        memory.store(facts)
        return {'type': 'stored'}
