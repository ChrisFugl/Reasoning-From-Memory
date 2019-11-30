"""Script to evaluate performance of the application."""

from app.language import get_language
from app.match import Matcher
import argparse
import evaluation
import json
import os
import pandas as pd
import sys
from tabulate import tabulate

def evaluate():
    """Evaluate system performance."""
    arguments = sys.argv[1:]
    options = _parse_arguments(arguments)
    config = _parse_config(options.config)
    threholds = config['threholds']
    evaluation_results = _run_evaluations(threholds['fact'], threholds['question'])
    _print_summary(evaluation_results)
    _print_total(evaluation_results)
    _save_results(config['save_directory'], evaluation_results)

def _parse_arguments(arguments):
    parser = argparse.ArgumentParser()
    parser.add_argument('config', type=str, help='path to config')
    options = parser.parse_args(arguments)
    return options

def _parse_config(config_path):
    with open(config_path, 'r') as config_file:
        config = json.load(config_file)
    config['name'] = config_path.split('/')[-1].replace('.json', '')
    config['save_directory'] = f'results/{config["name"]}'
    return config

def _run_evaluations(threshold_fact, threshold_question):
    nlp = get_language()
    matcher = Matcher(nlp)
    print('Evaluating: Answer no.')
    answer_no_result = evaluation.answer_no(threshold_fact, matcher)
    print('Evaluating: Answer unknown.')
    answer_unknown_result = evaluation.answer_unknown(threshold_question, matcher)
    print('Evaluating: Answer using inference.')
    answer_with_inference_result = evaluation.answer_with_inference(threshold_question, matcher)
    print('Evaluating: Answer without using inference.')
    answer_without_inference_result = evaluation.answer_without_inference(threshold_question, matcher)
    print('Evaluating: Match sentence with itself.')
    match_to_self_result = evaluation.match_to_self(threshold_fact, matcher)
    print('Evaluating: No match for unknown facts.')
    no_match_for_unknown_facts_result = evaluation.no_match_for_unknown_facts(threshold_fact, matcher)
    print('Evaluating: No match for unknown questions.')
    no_match_for_unknown_questions_result = evaluation.no_match_for_unknown_questions(threshold_question, matcher)
    return [
        answer_no_result,
        answer_unknown_result,
        answer_with_inference_result,
        answer_without_inference_result,
        match_to_self_result,
        no_match_for_unknown_facts_result,
        no_match_for_unknown_questions_result,
    ]

def _print_summary(evaluation_results):
    rows = list(map(_make_summary_row, evaluation_results))
    headers = ['Test', 'Accuracy', 'Failed', 'Passed']
    print('')
    print('Summary:')
    print(tabulate(rows, headers, tablefmt='fancy_grid'))

def _make_summary_row(evaluation_result):
    """
    Format evaluation result in a row format.

    :type evaluation_result: evaluation.types.EvaluationResult
    :rtype: (str, str, str, str)
    """
    accuracy = f'{evaluation_result.accuracy:0.2%}'
    failed = str(evaluation_result.number_of_failed)
    passed = str(evaluation_result.number_of_passed)
    return (evaluation_result.name, accuracy, failed, passed)

def _print_total(evaluation_results):
    headers = ['Tests', 'Accuracy', 'Failed', 'Passed']
    totals_rw = _get_totals_row(evaluation_results)
    print('')
    print('Total:')
    print(tabulate([totals_rw], headers, tablefmt='fancy_grid'))

def _get_totals_row(evaluation_results):
    number_of_failed = 0
    number_of_passed = 0
    total = 0
    for evaluation_result in evaluation_results:
        number_of_failed += evaluation_result.number_of_failed
        number_of_passed += evaluation_result.number_of_passed
        total += evaluation_result.total
    if total == 0:
        accuracy = 0.0
    else:
        accuracy = number_of_passed / total
    accuracy = f'{accuracy:0.2%}'
    return total, accuracy, str(number_of_failed), str(number_of_passed)

def _save_results(save_directory, evaluation_results):
    print('')
    print(f'Saving results in "{save_directory}".')
    os.makedirs(save_directory, exist_ok=True)
    columns = ['name', 'description', 'passed', 'fail_reason']
    for evaluation_result in evaluation_results:
        save_path = os.path.join(save_directory, f'{evaluation_result.name}.csv')
        evaluation_data = list(map(_make_save_row, evaluation_result.test_results))
        dataframe = pd.DataFrame(evaluation_data, columns=columns)
        dataframe.to_csv(save_path, index=False)

def _make_save_row(test_result):
    if test_result.fail_reason is None:
        fail_reason = 'null'
    else:
        fail_reason = test_result.fail_reason
    return [test_result.name, test_result.description, test_result.passed_binary, fail_reason]

if __name__ == '__main__':
    evaluate()
