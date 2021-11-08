# Base class for tests

import sys

from yaspin import yaspin

from slog.common.client import SlogClient
import slog.protobufs.slog_pb2 as slog_pb2
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc

class Test:
    """
    Base Test class
    """
    def __init__(self, server, txt):
        self.test_text = txt
        self.spin_text = self.test_text
        self.client = SlogClient('{}:5108'.format(server))

    def success(self):
        """
        On test success call this
        """
        print('\033[32m ✔ Success \033[0m')
        sys.exit(0)

    def fail(self, msg=""):
        """
        On test failure call this
        """
        if msg != "":
            msg = ": " + msg
        print('\033[31;1m 💥 Failure{} \033[0m'.format(msg))
        sys.exit(1)

    def run_test(self, writer):
        """
        Override this
        """
        return True

    def test(self):
        """
        Starts the test
        """
        with yaspin(text=self.spin_text) as spinner:
            if self.run_test(spinner):
                spinner.ok("✔")
            else:
                spinner.fail("💥")
