"""Methods related to asking a question."""

from app.parse import parse
from app.reason import reason
from app.utils import answer2message, match2question

def ask(threshold, matcher, facts, message):
    """
    Ask a question.

    :type threshold: float
    :type matcher: app.match.Matcher
    :type facts: list of app.types.fact.Fact
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
        if 1 < n_matches:
            return {'type': 'select_match', matches: matches}
        else:
            selected_match = matches[0]
            question = match2question(selected_match)
            answer = reason(facts, question)
            reply = answer2message(answer)
            return {'type': 'answer', 'answer': reply}
