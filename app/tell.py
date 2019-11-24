"""Methods related to processing a fact."""

import app.io as io
from app.match import input2matches
from app.types.fact import Fact

def tell(memory, threshold):
    """
    Interactive option to tell a fact.

    :type memory: app.memories.memory.Memory
    :type threshold: float
    """
    io.reply('What do you want to tell me?')
    while True:
        user_input, valid, validation_mesage = io.prompt('ask')
        if not valid:
            io.reply(validation_mesage)
            continue
        matches = input2matches(user_input, threshold)
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
