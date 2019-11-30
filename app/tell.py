"""Methods related to processing a fact."""

import app.io as io
from app.types.fact import Fact

def tell(threshold, matcher, memory):
    """
    Interactive option to tell a fact.

    :type threshold: float
    :type matcher: app.match.Matcher
    :type memory: app.memories.memory.Memory
    """
    io.reply('What do you want to tell me?')
    while True:
        user_input, valid, validation_mesage = io.prompt('tell')
        if not valid:
            io.reply(validation_mesage)
            continue
        matches = matcher.input2matches(threshold, user_input)
        n_matches = len(matches)
        if n_matches == 0:
            # TODO: how to give detailed information on why no match was found?
            io.reply('Sorry. My digital brain is not yet fully evolved. I did not understand it.')
            io.reply('Could you rephrase the the fact, so I might be able to understand it?')
        else:
            facts = list(map(_match2fact, matches))
            memory.store(facts)
            io.reply('Got it! I will remember that.')
            return

def _match2fact(match):
    return Fact(match.relation)
