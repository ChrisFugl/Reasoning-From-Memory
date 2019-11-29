"""Word Mover's Distance to compute distance between two sentences."""

def distance(sentence, target_sentence):
    """
    Word Mover's Distance between sentence and target sentence.

    :type sentence: str
    :type target_sentence: str
    :rtype: float
    """
    # TODOL: implement word mover's distance
    return len(set(target_sentence) - set(sentence)) + len(set(sentence) - set(target_sentence))
