"""Methods related to asking a question."""

import app.io as io
from app.reason import reason
from app.types.answer import Answer
from app.types.question import Question

def ask(threshold, matcher, facts):
    """
    Interactive option to ask a question.

    :type threshold: float
    :type matcher: app.match.Matcher
    :type facts: list of app.types.fact.Fact
    """
    io.reply('What do you want to ask me?')
    while True:
        user_input, valid, validation_mesage = io.prompt('ask')
        if not valid:
            io.reply(validation_mesage)
            continue
        matches = matcher.input2matches(threshold, user_input)
        n_matches = len(matches)
        if n_matches == 0:
            # TODO: how to give detailed information on why no match was found?
            io.reply('Sorry. My digital brain is not yet fully evolved. I did not understand it.')
            io.reply('Could you rephrase the question, so I might be able to understand it?')
            return
        else:
            if 1 < n_matches:
                selected_match = _select_match(matches)
            else:
                selected_match = matches[0]
            question = _match2question(selected_match)
            answer = reason(facts, question)
            _print_answer(answer)
            return

def _select_match(matches):
    """
    Present all possible matches to user and prompt user to pick one of them.

    :type matches: list of app.types.match.Match
    :rtype: app.types.match.Match
    """
    # TOOD: add user friendly representation to of each relation
    raise NotImplementedError()

def _match2question(match):
    return Question(match.relation)

def _print_answer(answer):
    if answer.answer == Answer.UNKNOWN:
        return io.reply('I do not know.')
    elif answer.answer == Answer.NO:
        return io.reply('No.')
    else:
        return io.reply('Yes.')
