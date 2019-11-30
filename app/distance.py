"""Word Mover's Distance to compute distance between two sentences."""

def distance(nlp, sentence1, sentence2):
    """
    Word Mover's Distance between sentence and target sentence.

    :type nlp: spacy.language.Language
    :type sentence1: str
    :type sentence2: str
    :rtype: float
    """
    document1 = nlp(sentence1)
    document2 = nlp(sentence2)
    # similarity is actually a distance, which is a bit confusing
    distance = document1.similarity(document2)
    return distance
