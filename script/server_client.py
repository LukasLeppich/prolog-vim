import os
import re
import json
import threading
import time
import subprocess
import model
import stream_utils


class LSPServer(object):
    header_regex = re.compile(r'Content-Length: (\d+)')

    def __init__(self, project):
        self.is_initialized = False
        self.server_command = None
        self.project = project
        self.process = None
        self.response_handler = {}
        self.server_capabilities = {}

    def initialize(self):
        req = model.RequestMessage('initialize',
                                   model.InitializeParams(os.getpid(),
                                                          self.project.dir))
        response = Response()
        response.set_callback(self._initialize_response)
        self._send_request(req, response)
        return response

    def _initialize_response(self, response):
        self.is_initialized = True
        if response.has_error():
            print('Error while initialize request')
            print(response.error)
        else:
            message = response.data
            if 'capabilities' in message:
                self.server_capabilities = message['capabilities']
            else:
                print('No server capabilities in initialize response')
                print(message)

    def start_server(self, command):
        self.server_command = command
        self._start_server()

    def _start_server(self):
        self.process = subprocess.Popen(self.server_command,
                                        cwd=self.project.dir,
                                        # No buffer
                                        bufsize=0,
                                        stdin=subprocess.PIPE,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE,
                                        # Not handling new line
                                        universal_newlines=False)
        self._attach_stream_reader()
        self.initialize()

    def stop_server(self):
        req = model.RequestMessage('shutdown')
        response = Response()
        response.set_callback(self._exit_server)
        self._send_request(req, response)
        return response

    def _exit_server(self):
        req = model.RequestMessage('exit')
        self._send_request(req)

    def did_open(self, name, content):
        req = model.RequestMessage('textDocument/didOpen',
                                   model.DidOpenTextDocumentParams(name,
                                                                   'prolog',
                                                                   1,
                                                                   content))
        response = Response()
        self._send_request(req, response)
        return response

    def did_change(self, name, content):
        changes = [
            model.TextDocumentContentChangeEvent(content)
        ]
        req = model.RequestMessage('textDocument/didChange',
                                   model.DidChangeTextDocumentParams(name,
                                                                     1,
                                                                     changes))
        response = Response()
        self._send_request(req, response)
        return response

    def completion(self, name, line, character):
        position = model.Position(line, character)
        document = model.TextDocumentIdentifier(name)
        req = model.RequestMessage('textDocument/completion',
                                   model.TextDocumentPositionParams(document,
                                                                    position))
        response = Response()
        print('Send completion request')
        self._send_request(req, response)
        return response

    def _attach_stream_reader(self):
        input_thread = threading.Thread(target=self._read_input_stream)
        error_thread = threading.Thread(target=self._read_error_stream)
        server_check_thread = threading.Thread(target=self._check_server)
        input_thread.setDaemon(True)
        error_thread.setDaemon(True)
        server_check_thread.setDaemon(True)
        input_thread.start()
        error_thread.start()
        server_check_thread.start()

    def _check_server(self):
        while self.process.poll() is None:
            time.sleep(0.5)
        for _, handler in list(self.response_handler.items()):
            handler.set_error('Server is offline')
        print('Server is offline!')
        self._start_server()

    def _read_error_stream(self):
        stream_helper = stream_utils.LSPStream(self.process.stderr)
        while self.process.poll() is None:
            time.sleep(1)
            pass
            #print('ERROR: ' + stream_helper.read_line())
        print('Stop reading error stream')

    def _read_input_stream(self):
        streamHelper = stream_utils.LSPStream(self.process.stdout)
        headers = []
        while self.process.poll() is None:
            line = streamHelper.read_line()
            if len(line) == 0 and len(headers) > 0:
                length = self._get_header_length(headers)
                del headers[:]
                self._handle_response(streamHelper.read_bytes(int(length)))
            else:
                headers.append(line)
        print('server stopped!')
        for _, handler in list(self.response_handler.items()):
            handler.set_error('Server is offline')

    def _get_header_length(self, headers):
        for header in headers:
            match = LSPServer.header_regex.match(header)
            if match:
                return match.group(1)

    def _handle_response(self, json_string):
        message = json.loads(json_string)
        if 'id' in message:
            request_id = message['id']
            if request_id in self.response_handler:
                if 'result' in message:
                    self.response_handler[request_id].set_data(message['result'])
                    del self.response_handler[request_id]
                elif 'error' in message:
                    self.response_handler[request_id].set_error(message['error'])
                    del self.response_handler[request_id]
            else:
                if 'error' in message:
                    print('Response error: ')
                    print(message['error'])
                print('No response handler for id ' + request_id)
        else:
            print('json has no id property: ')
            print(message)

    def _send_request(self, request, response=None):
        if response is not None:
            self.response_handler[request.request_id] = response
        json_string = request.to_json()
        content = 'Content-Length: %d\r\n\r\n%s' % (len(json_string),
                                                    json_string)
        try:
            self.process.stdin.write(content.encode('utf8'))
        except Exception as error:
            if response is not None:
                response.set_error(error)


class Response(object):
    def __init__(self):
        self._lock = threading.Lock()
        self._lock.acquire()
        self.data = None
        self.error = None
        self.callback = None

    def set_callback(self, cb):
        self.callback = cb

    def wait_for_response(self):
        print('wait for lock')
        self._lock.acquire()
        print('lock acquire')
        return self

    def has_error(self):
        return self.error is not None

    def set_data(self, data):
        self.data = data
        if self.callback is not None:
            self.callback(self)
        if self._lock.locked():
            self._lock.release()

    def set_error(self, error):
        self.error = error
        if self.callback is not None:
            self.callback(self)
        if self._lock.locked():
            self._lock.release()
