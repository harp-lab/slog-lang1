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
TEST_DIR = "/slog/slog/tests/testcase"

class SizeCompareTest(Test):
    """
    Test if backend compiler produce correct size
    """
    def __init__(self):
        super().__init__("localhost", ("Backend integration test"))

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
        checkpoint_path = f"{WORKDIR}/out/checkpoints/checkpoint-final"
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
        for test_name in os.listdir(TEST_DIR):
            writer.write(f"Now testing {test_name} ...")
            testcase_dir = f'{TEST_DIR}/{test_name}'
            with open(f'{testcase_dir}/ground_truth') as truth_file:
                for line in truth_file:
                    rel_name = line.split(',')[0].strip()
                    arity = line.split(',')[1].strip()
                    expected = int(line.split(',')[2].strip())
                    self.check_count(f'{testcase_dir}/{test_name}.slog',
                                     f'{testcase_dir}/input',
                                     rel_name, arity, expected)

SizeCompareTest().test()
