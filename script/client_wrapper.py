import sys
import json
import vim

sys.path.append(vim.eval('s:plugin') + '/script')
import client


def init_project(workspace):
    client.init_project(workspace)


def start_server(command):
    client.start_server(command)


def is_server_running():
    return client.is_server_running()


def stop_server():
    client.stop_server()


def did_open(name, content):
    client.did_open(name, content)


def did_change(name, content):
    client.did_change(name, content)


def completion(name, line_string, character_string):
    character = int(character_string)
    line = int(line_string)
    completion_list = client.completion(name, line, character)
    current_line = vim.current.buffer[line - 1]
    start = -2
    if len(completion_list) > 0 and len(current_line) > 0:
        word = completion_list[0]
        best = -1
        for i in range(character - 2, -1, -1):
            prefix = current_line[i:character]
            if word.startswith(prefix):
                best = i
        if best > -1:
            start = best
    vim.command('let b:prologCompletionStart=' + str(start))
    vim.command('let b:prologCompletionList=' +
                json.dumps(completion_list))
