"""
Compile and run a small file, and then dump its output to the console
"""

import os

from slog.tests.test import Test

FILE = "tc.slog"

class DumpTest(Test):
    """
    Tests if the correct hash is given to the input tc.slog file.
    """
    def __init__(self):
        super().__init__("localhost", ("Compiling a valid file should return" +
                                       "hash for initial DB, when recompiled " +
                                       "should return that same hash."))

    def run_test(self, writer):
        slogpath = os.path.join(os.path.dirname(__file__), FILE)
        initial_db = self.client.compile_slog(slogpath, writer)
        writer.write('Compilation finished, checking against known digest')
        # hashes of a file are deterministic, so it should compile to this hash.
        # if its not this, either something has gone horribly wrong or we started using
        # a different message digest algorithm.
        expected = 'ea15f5ea1c5a51e39dfa1f532b98f78394267b76d386a0b86c3813abe61a2f82'
        if expected == initial_db:
            self.success("hashes matched")
        else:
            self.fail("hashes didn't match.")

DumpTest().test()
