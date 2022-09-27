"""
A CI test for a slog compiler only check the output size
Yihao Sun
"""

from argparse import ArgumentParser
import os
import subprocess
import shutil
from slog.daemon.util import get_relation_info

from slog.tests.test import Test

MYDIR = os.path.dirname(os.path.abspath(__file__))
RUNSLOG_PATH = f"{MYDIR}/../../runslog"
WORKDIR = f"{MYDIR}/../.."
TEST_DIR = f"{MYDIR}/testcase"

class SizeCompareTest(Test):
    """
    Test if backend compiler produce correct size
    """
    def __init__(self, cores):
        self.cores = cores
        super().__init__("localhost", ("Backend integration test"))

    def check_count(self, test_name, slogpath, factpath, expected_counts):
        out_dir = f"{WORKDIR}/out"
        if os.path.exists(out_dir):
            shutil.rmtree(out_dir)
        if not os.path.exists(out_dir):
            os.makedirs(out_dir)
        try:
            exec_out = subprocess.check_output(
                [RUNSLOG_PATH, "-ov", "-v", "-j", str(self.cores),
                "-f", factpath, slogpath, out_dir],
                 stderr=subprocess.STDOUT)
            print(exec_out.decode())
        except subprocess.CalledProcessError as e:
            self.fail(f"Slog file failed! Code: `{e.returncode}`, Error:\n{e.output.decode()}")
            return False
        # for manual inspection:
        # print("copying to " + f'{tempdir}/slog-tests/{test_name}')
        # shutil.copytree(out_dir, f'{tempdir}/slog-tests/{test_name}')
        checkpoint_path = f"{out_dir}/checkpoints/checkpoint-final"
        for (rel_name, arity, expected_count) in expected_counts:
            out_found = False
            count = 0 
            for fp in os.listdir(checkpoint_path):
                if fp.find(f".{rel_name}.{arity}") > 0:
                    out_found = True
                    count = get_relation_info(os.path.join(checkpoint_path, fp))['num_tuples']
                    break
            file_exists_msg = "" if out_found else " (facts file does not exist)"
            if count != expected_count:
                self.fail(f"{test_name}: {rel_name} should have {expected_count} facts, but has {count}{file_exists_msg}")
            else:
                self.success(f"{test_name}: {rel_name} has {count} facts, as expected{file_exists_msg}")

    def run_test(self, writer):
        for test_name in os.listdir(TEST_DIR):
            writer.write(f"Now testing {test_name} ...")
            testcase_dir = f'{TEST_DIR}/{test_name}'
            with open(f'{testcase_dir}/ground_truth') as truth_file:
                expected_counts = []
                for line in truth_file:
                    if line.strip() == "" or line.strip().startswith("#"): continue
                    rel_name = line.split(',')[0].strip()
                    arity = line.split(',')[1].strip()
                    expected = int(line.split(',')[2].strip())
                    expected_counts.append((rel_name, arity, expected))
                self.check_count(test_name,
                                    f'{testcase_dir}/{test_name}.slog',
                                    f'{testcase_dir}/input',
                                    expected_counts)
        # if os.path.exists(f"{WORKDIR}/out"):
        #     shutil.rmtree(f"{WORKDIR}/out")
        # self.success()

if __name__ == "__main__":
    parser = ArgumentParser("Fact size compare testing.")
    parser.add_argument("--cores", help="number of cores to run the test",
                        type=int, dest="cores", default=1)
    args = parser.parse_args()
    SizeCompareTest(args.cores).test()
