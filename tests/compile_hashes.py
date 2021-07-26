# Compiling a valid slog file should return a hash for the initial
# database, and when done again it should return the same hash.
from tests.test import *
import os
import time
from repl.elaborator import *
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

FILE = "tc.slog"

PING_INTERVAL = .1
STATUS_PENDING  = 0
STATUS_FAILED   = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = -1

class CompileHashesTest(Test):
    def __init__(self):
        super().__init__("localhost", "Compiling a valid file should return hash for initial DB, when recompiled should return that same hash.")
    
    def compile_file(self,filename):
        path = os.path.join(os.getcwd(),FILE)
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
            if (res.status == STATUS_PENDING):
                continue
            elif (res.status == STATUS_FAILED):
                self.fail("compilation failed.")
            elif (res.status == STATUS_RESOLVED):
                initial_db = res.err_or_db
                break
        return initial_db

    def run_test(self):
        #print('Compiling once...')
        initial_db = self.compile_file(FILE)
        print('Compiling again...')
        initial_db_other = self.compile_file(FILE)
        if initial_db_other == initial_db:
            self.success()
        else:
            self.fail("hashes didn't match.")


CompileHashesTest().test()
