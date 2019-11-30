"""Contains language functions."""

import spacy
import wmd

def get_language():
    """
    Initialize a language instance.

    :rtype: spacy.language.Language
    """
    nlp = spacy.load('en_core_web_md')
    # we are not interested in stop-words as most of them are
    # needed in the short sentence examples in relation definitions
    spacy_wmd_hook = wmd.WMD.SpacySimilarityHook(nlp, ignore_stops=False)
    nlp.add_pipe(spacy_wmd_hook, last=True)
    return nlp
