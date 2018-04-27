import json
import os


class Project(object):
    def __init__(self, directory):
        self.dir = self.__find_project_root(os.path.normpath(directory))
        self.port = None
        self.proc = None
        self.last_failed = 0
        self.process = None

    def __find_project_root(self, directory, default=None):
        if os.path.exists(os.path.join(directory, 'prolog.json')):
            return directory
        if default is None:
            default = directory
        if os.path.dirname(directory) == directory:
            return default
        return self.__find_project_root(os.path.dirname(directory),
                                        default=default)


class APIObject(object):
    def to_dict(self):
        return self.__dict__


class Message(APIObject):
    def __init__(self):
        self.jsonrpc = '2.0'

    def to_json(self):
        return json.dumps(self.to_dict())


class RequestMessage(Message):
    __nextId = 1

    def __init__(self, method, params=APIObject()):
        super(RequestMessage, self).__init__()
        self.request_id = RequestMessage.__nextId
        RequestMessage.__nextId += 1
        self.method = method
        self.params = params

    def to_dict(self):
        return {
            'id': self.request_id,
            'method': self.method,
            'params': self.params.to_dict()
        }


class Position(APIObject):
    def __init__(self, line, character):
        self.line = line
        self.character = character


class Range(APIObject):
    def __init__(self, start, end):
        self.start = start
        self.end = end

    def to_dict(self):
        return {
            'start': self.start.to_dict(),
            'end': self.end.to_dict()
        }


class Location(APIObject):
    def __init__(self, uri, char_range):
        self.uri = uri
        self.range = char_range

    def to_dict(self):
        return {
            'uri': self.uri,
            'range': self.range.to_dict()
        }


class TextDocumentIdentifier(APIObject):
    def __init__(self, uri):
        self.uri = uri

    def to_dict(self):
        return {
            'uri': self.uri
        }


class TextDocumentItem(APIObject):
    def __init__(self, uri, language_id, version, text):
        self.uri = uri
        self.language_id = language_id
        self.version = version
        self.text = text

    def to_dict(self):
        return {
            'uri': self.uri,
            'languageId': self.language_id,
            'version': self.version,
            'text': self.text
        }


class VersionedTextDocumentIdentifier(TextDocumentIdentifier):
    def __init__(self, uri, version):
        super(VersionedTextDocumentIdentifier, self).__init__(uri)
        self.version = version

    def to_dict(self):
        output = super(VersionedTextDocumentIdentifier, self).to_dict()
        output['version'] = self.version
        return output


class TextDocumentPositionParams(APIObject):
    def __init__(self, text_document, position):
        self.text_document = text_document
        self.position = position

    def to_dict(self):
        return {
            'textDocument': self.text_document.to_dict(),
            'position': self.position.to_dict()
        }


class TextDocumentContentChangeEvent(APIObject):
    def __init__(self, text, content_range=None, range_length=None):
        self.text = text
        if content_range is not None:
            self.range = content_range
        if range_length is not None:
            self.range_length = range_length

    def to_dict(self):
        response = {'text': self.text}
        if hasattr(self, 'range'):
            response['range'] = self.range
        if hasattr(self, 'rangeLength'):
            response['rangeLength'] = self.range_length
        return response


class InitializeParams(APIObject):
    def __init__(self, process_id, root_path,
                 initialization_options={},
                 capabilities={}):
        self.process_id = process_id
        self.root_path = root_path
        self.initialization_options = initialization_options
        self.capabilities = capabilities

    def to_dict(self):
        response = {}
        if hasattr(self, 'root_path'):
            response['rootPath'] = self.root_path
        if hasattr(self, 'process_id'):
            response['processId'] = self.process_id
        if hasattr(self, 'initializationOptions'):
            response['initializationOptions'] = self.initialization_options
        if hasattr(self, 'capabilities'):
            response['capabilities'] = self.capabilities
        return response


class DidOpenTextDocumentParams(APIObject):
    def __init__(self, uri, language_id, version, text):
        self.text_document = TextDocumentItem(uri, language_id, version, text)

    def to_dict(self):
        return {
            'textDocument': self.text_document.to_dict()
        }


class DidChangeTextDocumentParams(APIObject):
    def __init__(self, uri, version, changes):
        self.text_document = VersionedTextDocumentIdentifier(uri, version)
        self.content_changes = changes

    def to_dict(self):
        changes = list(map(lambda change: change.to_dict(),
                           self.content_changes))
        return {
            'textDocument': self.text_document.to_dict(),
            'contentChanges': changes
        }
