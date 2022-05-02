"""
Compiling a valid slog file should return a hash for the initial
database, and when done again it should return the same hash.
"""

import os

from slog.tests.test import Test

FILE = "tc.slog"

class CompileHashesTest(Test):
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

CompileHashesTest().test()
