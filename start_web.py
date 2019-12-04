"""Run this script to start the digital brain."""

from app.ask import ask
from app.language import get_language
from app.match import Matcher
from app.memories.persistent import PersistentMemory
from app.tell import tell
import json
import sys

_CONFIG_PATH = 'configs/web.json'

def _main():
    config = _parse_config()
    thresholds = config['threholds']
    memory = PersistentMemory(config['memory_path'])
    nlp = get_language()
    matcher = Matcher(nlp)
    print(json.dumps({'type': 'finished_loading'}))
    while True:
        user_input = sys.stdin.readline()
        request = json.loads(user_input)
        message_type = request['type']
        if message_type == 'exit':
            break
        elif message_type == 'is_loading':
            response = {'type': 'finished_loading'}
        elif message_type == 'ask':
            response = ask(thresholds['question'], matcher, memory.facts, request['message'])
        elif message_type == 'ask_select_match':
            response = _ask_select_match(memory.facts, request['match'])
        elif message_type == 'list_facts':
            response = _list_facts(memory.facts)
        elif message_type == 'tell':
            response = tell(thresholds['fact'], matcher, memory, request['message'])
        else:
            raise Exception(f'Unknown message type: {message_type}')
        print(json.dumps(response))
        sys.stdout.flush()

def _parse_config():
    with open(_CONFIG_PATH, 'r') as config_file:
        config = json.load(config_file)
    return config

def _ask_select_match(facts, match):
    raise NotImplementedError()

def _list_facts(facts):
    raise NotImplementedError()

if __name__ == '__main__':
    _main()
