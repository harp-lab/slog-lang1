import grpc
from concurrent import futures
import time
from prompt_toolkit import prompt
from prompt_toolkit.formatted_text import HTML
from sexpdata import loads, dumps
from repl.parser import *

import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

class Repl:
    def __init__(self):
        self._channel = None
        self._parser = CommandParser()
        self.reconnect("localhost")

    def add_time(self,uuid):
        pass

    def connected(self):
        return self._channel != None
    
    def reconnect(self,server):
        self._server = server
        self._channel = grpc.insecure_channel('{}:5106'.format(server))
        self._stub = slog_pb2_grpc.CommandServiceStub(self._channel)
        self._cur_time = 0
        self.num_local_times = 0
        self.time_prev = {}
        self.time_uuids = {}

    def load(self,path):
        with open(path,'r') as f:
            content = f.read()
            req = slog_pb2.LoadProgramReq()
            req.session_key = "empty"
            req.source_program = content
            response = self._stub.LoadProgram(req)

    def get_front(self):
        if (not self.connected()):
            return "Disconnected"
        elif (self._cur_time == 0):
            return "⊥"
        else:
            return self.time

    def loop(self):
        while True:
            try:
                front = self.get_front()
                text = prompt('σλoγ [{}] » '.format(front), bottom_toolbar=self.bottom_toolbar())
                cmd = self._parser.parse(text)
                cmd.execute(self)
            except EOFError:
                self.exit()
            except AssertionError:
                return
            
    def exit(self):
        print('Goodbye.')
        exit(0)

    def bottom_toolbar(self):
        if self.connected():
            return HTML('<style color="lightgreen">[host: <b>{}</b>]  [?? jobs in queue]</style>'.format(self._server))
        else:
            return HTML('Disconnected. Use `connect host`')

repl = Repl()

try:
    while True:
        repl.loop()
except KeyboardInterrupt:
    repl.exit()
