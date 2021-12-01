"""
A CI test for a slog compiler only check the output size

Yihao Sun
"""

import os
from posixpath import relpath
import subprocess
import shutil
from slog.daemon.util import get_relation_info

from slog.tests.test import Test

RUNSLOG_PATH = "/slog/runslog"
WORKDIR = "/slog"

class SizeCompareTest(Test):
    """
    Test if backend compiler produce correct size
    """


    def check_count(self, slogpath, factpath, rel_name, arity, expected_count):
        """ transitive closure """
        if os.path.exists(f"{WORKDIR}/out"):
            shutil.rmtree(f"{WORKDIR}/out")
        try:
            exec_out = subprocess.check_output(
                [RUNSLOG_PATH, "-j", "4", "-f", factpath, slogpath, f"{WORKDIR}/out"],
                cwd=WORKDIR, stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as e:
            self.fail(f"Slog file failed! Code: `{e.returncode}`, Error:\n{e.output.decode()}")
            # return False
        checkpoint_path = f"{WORKDIR}/out/checkpoint/checkpoint-final"
        out_found = False
        for fp in os.listdir(checkpoint_path):
            if fp.find(f"{rel_name}.{arity}"):
                out_found = True
                count = get_relation_info(os.path.join(checkpoint_path, fp))['num_tuples']
                if count != expected_count:
                    self.fail(f"path should have {expected_count}, but get {count}")
                else:
                    self.success()
        if not out_found:
            self.fail(f"path should have {expected_count}, but get 0")

    def run_test(self, writer):
        """"""
        writer.write("testing `tc` ...")
        self.check_count(
            f"{WORKDIR}/compiler/tests/tc.slog",
            f"{WORKDIR}/slog/test/csv/tc",
            "path", 2,
            219126)
        writer.write("testing `deftest` ...")
        self.check_count(
            f"{WORKDIR}/compiler/tests/deftest.slog",
            f"{WORKDIR}/slog/test/csv/cgc_def",
            "def_used", 4,
            7128)

SizeCompareTest.test()
