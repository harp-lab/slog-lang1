import grpc
from concurrent import futures
import time
from prompt_toolkit import prompt
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.formatted_text import HTML
from sexpdata import loads, dumps

import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

class Repl:
    def __init__(self):
        self._channel = grpc.insecure_channel('localhost:5106')
        self._stub = slog_pb2_grpc.CommandServiceStub(self._channel)

    def loop(self):
        text = prompt('σλoγ >', bottom_toolbar=self.bottom_toolbar())
        sexp = loads(text)

    def bottom_toolbar(self):
        return HTML('server: <b><style bg="ansired">??</style></b> ?? jobs in queue. Last cmd took ?? seconds.')

repl = Repl()

try:
    while True:
        repl.loop()
except KeyboardInterrupt:
    print('Goodbye.')
