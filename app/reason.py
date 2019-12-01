"""Contains function to answer user questions."""

from app.types.answer import Answer
from app.utils import name2relation
import os
import subprocess

_RULES_PATH = os.path.abspath('app/relations/relations.pl')

def reason(facts, question):
    """
    Answer a user question.

    :type facts: list of app.types.fact.Fact
    :type question: app.types.question.Question
    :rtype: app.types.answer.Answer
    """
    knowledge_base = _facts2knowledge_base(facts)
    query = question.relation.as_prolog_query() + '.'
    passed_query = _query(knowledge_base, query)
    if passed_query:
        return Answer(Answer.YES, question)
    else:
        negated_query = _get_negated_query(question)
        passed_negated_query = _query(knowledge_base, negated_query)
        if passed_negated_query:
            return Answer(Answer.NO, question)
        else:
            return Answer(Answer.UNKNOWN, question)

def _facts2knowledge_base(facts):
    prolog_facts = map(_fact_as_prolog, facts)
    knowledge_base_facts = ' '.join(prolog_facts)
    knowledge_base = f'[user]. {knowledge_base_facts} end_of_file.'
    return knowledge_base

def _fact_as_prolog(fact):
    return f'{fact.relation.as_prolog_fact()}.'

def _get_negated_query(question):
    relation = question.relation
    relation_name = relation.relation_name
    if relation_name.endswith('_negated'):
        negated_relation_name = relation_name.replace('_negated', '')
    else:
        negated_relation_name = f'{relation_name}_negated'
    negated_relation_class = name2relation(negated_relation_name)
    negated_relation = negated_relation_class(relation.entity1, relation.entity2)
    return negated_relation.as_prolog_query() + '.'

def _query(knowledge_base, query):
    prolog_input = f'{knowledge_base} {query}'
    process = subprocess.run(['swipl', _RULES_PATH], stdout=subprocess.PIPE, stderr=subprocess.PIPE, input=prolog_input, encoding='utf-8')
    assert process.returncode == 0
    passed = _parse_prolog_output(process.stdout)
    return passed

def _parse_prolog_output(stdout):
    lines = stdout.split('\n')
    processed_lines = map(_process_prolog_line, lines)
    non_empty_lines = list(filter(_is_not_empty, processed_lines))
    assert len(non_empty_lines) == 2
    # knowledge base should always return true
    assert non_empty_lines[0] == 'true'
    result = non_empty_lines[1]
    assert result in ['false', 'true']
    return result == 'true'

def _process_prolog_line(line):
    return line.lower().strip().replace('.', '')

def _is_not_empty(line):
    return len(line) != 0
