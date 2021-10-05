"""
Compiling a valid slog file should return a hash for the initial
database, and when done again it should return the same hash.
"""

import os
import time

from slog.common.elaborator import Elaborator
import slog.protobufs.slog_pb2 as slog_pb2
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc
from slog.tests.test import Test

FILE = "tc.slog"

PING_INTERVAL = 1
STATUS_PENDING = 0
STATUS_FAILED = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = -1

class CompileHashesTest(Test):
    """
    Tests if the correct hash is given to the input tc.slog file.
    """
    def __init__(self):
        super().__init__("localhost", ("Compiling a valid file should return" +
                                       "hash for initial DB, when recompiled " +
                                       "should return that same hash."))

    def compile_file(self, filename, spinner):
        path = os.path.join(os.path.dirname(__file__), FILE)
        elaborator = Elaborator()
        elaborator.elaborate(path)
        req = slog_pb2.HashesRequest()
        req.session_key = "empty"
        req.hashes.extend(elaborator.hashes.keys())
        response = self._stub.ExchangeHashes(req)
        req = slog_pb2.PutHashesRequest()
        req.session_key = "empty"
        for hsh in response.hashes:
            req.bodies.extend([elaborator.hashes[hsh]])
        response = self._stub.PutHashes(req)
        req = slog_pb2.CompileHashesRequest()
        req.buckets = 4096
        req.using_database = ""
        req.hashes.extend(elaborator.hashes.keys())
        response = self._stub.CompileHashes(req)
        cmmt = None
        initial_db = ""
        while True:
            time.sleep(PING_INTERVAL)
            p = slog_pb2.Promise()
            p.promise_id = response.promise_id
            res = self._stub.QueryPromise(p)
            if res.status == STATUS_FAILED:
                spinner.write(f'FAILED! {res}')
                self.fail("compilation failed.")
            elif res.status == STATUS_RESOLVED:
                spinner.write(f'Resolved! {res}')
                initial_db = res.err_or_db
                break
            elif res.status != STATUS_PENDING:
                spinner.write(f'Something else! {res}')
        return initial_db

    def run_test(self, spinner):
        initial_db = self.compile_file(FILE, spinner)
        spinner.write('Compilation finished, checking against known digest')
        # hashes of a file are deterministic, so it should compile to this hash.
        # if its not this, either something has gone horribly wrong or we started using
        # a different message digest algorithm.
        expected = 'ea15f5ea1c5a51e39dfa1f532b98f78394267b76d386a0b86c3813abe61a2f82'
        if expected == initial_db:
            self.success()
        else:
            self.fail("hashes didn't match.")


CompileHashesTest().test()
