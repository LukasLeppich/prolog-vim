class LSPStream(object):
    def __init__(self, stream):
        self.__stream = stream

    def read_bytes(self, byte_count):
        return self.__stream.read(byte_count).decode('utf-8')

    def read_line(self):
        message = ''
        while True:
            value = self.__stream.read(1)
            if len(value) == 0:
                return message
            if value == b'\n':
                return message
            if value != b'\r':
                message += value.decode('utf-8')
