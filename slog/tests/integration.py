"""
Some integration test to test the behavior of
daemon + backend

Yihao Sun
"""

import os
import re
import sys
from multiprocessing import Process
from time import sleep

from slog.common.client import ConsoleWriter, SlogClient
import slog.daemon.server as server


def parse_test_result(test_file_path):
    """ parse a slog.test file return a map of query to result list """
    with open(test_file_path, 'r') as test_file:
        fdata = test_file.read()
    res = {}
    for case in fdata.split('\n\n'):
        # assume first line is always the query
        # assume always sperate blank line
        # WARNING: make sure this is true
        case = case.strip()
        query = case.split('\n')[0]
        result = re.findall(r'\(.+?\)', case[len(query):])
        res[query] = result
    return res


def test_slog(client: SlogClient, testcase_fpath, result_fpath, out_file):
    """ test a slog file """
    out_file.write(">>>>>>>>>>>>>>>>> NOW TESTING >>>>>>>>>>>>>>>>>\n")
    out_file.write(testcase_fpath+'\n')
    expected_res = parse_test_result(result_fpath)
    # compile and rule the slog file
    compile_res = client.compile_slog(testcase_fpath)
    if compile_res is None:
        out_file.write("Comiplation failed!\n")
        return
    inited_db = compile_res[0]
    client.run_with_db(testcase_fpath, inited_db)
    for query, ans in expected_res.items():
        server_res = client.pretty_print_slog_query('?'+query, ConsoleWriter())
        if server_res is None or set(server_res) != set(ans):
            out_file.write(f"query: {query}\n")
            out_file.write(f"expect: {ans}\n")
            out_file.write(f"but get: {server_res}\n")
        else:
            out_file.write(f"testcase {query} passed!\n")


def run_test(client: SlogClient, test_folder, out_fpath):
    with open(out_fpath, "w") as out_file:
        for slog_file in os.listdir(test_folder):
            if slog_file.endswith('.slog') and \
            os.path.exists(os.path.join(test_folder, f'{slog_file}.tests')):
                test_slog(client, os.path.join(test_folder, slog_file),
                          os.path.join(test_folder, f'{slog_file}.tests'),
                          out_file)


if __name__ == "__main__":
    # Take server as an optional argument to the repl.
    server_t = Process(target=server.run)
    server_t.start()
    sleep(2)
    sclient= SlogClient()
    run_test(sclient, "compiler/unit-tests", "./TESTOUT")
    print("\n>>>>>>>>>>>>>>>>>>>>>>>> TEST FINISH >>>>>>>>>>>>>>>>>>>>>\n")
    server_t.terminate()
    sys.exit(0)
