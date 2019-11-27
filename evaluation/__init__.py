"""
Set of tests designed to evaluate the performance of the application.

The tests are designed to evaluate the following aspects:
    * answer_no.py: Systems can respond with "No".
    * answer_unknown.py: System responds with ”I don’t know” for all relations that are not stored and can’t be inferred using the rules.
    * answer_with_inference.py: The rules can be used to infer an answer from the stored facts.
    * answer_without_inference.py If a fact is stored for any relation, a question can be asked to answer the fact (without using inference).
    * match_to_self.py: All of the sentences in a predefined relation should be matched to their corresponding relation.
    * no_match_for_unknown_facts.py: The system does not recognize facts that it should not be able to match to a relation.
    * no_match_for_unknown_questions/py: The system does not recognize questions that it should not be able to match to a relation.
"""

from evaluation.answer_no import answer_no
from evaluation.answer_unknown import answer_unknown
from evaluation.answer_with_inference import answer_with_inference
from evaluation.answer_without_inference import answer_without_inference
from evaluation.match_to_self import match_to_self
from evaluation.no_match_for_unknown_facts import no_match_for_unknown_facts
from evaluation.no_match_for_unknown_questions import no_match_for_unknown_questions
