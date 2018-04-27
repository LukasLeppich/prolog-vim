import model
from server_client import LSPServer


class Cache():
    server = None
    project = None

    @staticmethod
    def init_project(directory):
        Cache.project = model.Project(directory)

    @staticmethod
    def start_server(command):
        if Cache.project is None:
            raise ValueError('Project is not initialized')
        Cache.server = LSPServer(Cache.project)
        Cache.server.start_server(command)


def init_project(directory):
    Cache.init_project(directory)


def is_server_running():
    try:
        return hasattr(Cache.project, 'process') \
            and hasattr(Cache.project.process, 'poll')\
            and Cache.project.process.poll()
    except Exception as e:
        print(e)
        return False


def stop_server():
    Cache.server.stop_server()


def did_open(name, content):
    Cache.server.did_open(name, content)


def did_change(name, content):
    Cache.server.did_change(name, content)


def completion(name, line, character):
    response = Cache.server.completion(name, line, character)
    response.wait_for_response()
    if response.has_error():
        print('Error in completion request: ', response.error)
        return []
    else:
        return [item['label'] for item in response.data]


def start_server(command):
    try:
        Cache.start_server(command)
    except Exception as error:
        print(error)
        return None
