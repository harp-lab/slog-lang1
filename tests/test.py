# Base class for tests
from yaspin import yaspin

import grpc
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

class Test:
    def __init__(self,server,txt):
        self.test_text = txt
        self.reconnect(server)

    def success(self):
        print('\033[32m ✔ Success \033[0m')
        exit(0)

    def fail(self,msg=""):
        if msg != "":
            msg = ": " + msg
        print('\033[31;1m 💥 Failure{} \033[0m'.format(msg))
        exit(1)

    def reconnect(self,server):
        self._server = server
        self._channel = grpc.insecure_channel('{}:5108'.format(server))
        self._stub = slog_pb2_grpc.CommandServiceStub(self._channel)

    def run_test(self):
        return True

    def test(self):
        with yaspin(text=self.test_text) as spinner:
            if self.run_test():
                spinner.ok("✔")
            else:
                spinner.fail("💥")
