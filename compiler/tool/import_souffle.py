'''
a scrpit import souffle csv/tsv style EDB into slog input dir

Yihao Sun
'''

import argparse
import os

TSV2BIN_PATH = "../../parallel-RA/build/tsv_to_bin"


def get_arity(souffle_fpath):
    with open(souffle_fpath, 'r') as souffle_f:
        fst_line = souffle_f.readline()
    if fst_line.strip() == '':
        return -1
    else:
        return len(fst_line.strip().split('\t'))


def get_rel_name(souffle_fpath):
    return souffle_fpath.split('/')[-1][:-6]


def tsv_to_bin(souffle_fpath, slog_dir, buckets):
    arity = get_arity(souffle_fpath)
    rel_name = get_rel_name(souffle_fpath)
    slog_fname = f'{slog_dir}/{rel_name}_{arity}'
    if arity == -1:
        # skip empty file
        return
    else:
        index = ','.join([str(i) for i in range(1, arity+1)])
        os.system(' '.join([TSV2BIN_PATH, souffle_fpath,
                            str(arity), slog_fname, index, str(buckets),
                            slog_dir]))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="a scrpit import souffle tsv style EDB into slog input dir(keep index order)")
    parser.add_argument('souffle_dir', metavar='souffle_dir',
                        type=str, help='input souffle facts dir')
    parser.add_argument('slog_dir', metavar='slog_dir',
                        type=str, help="output slog facts dir")
    parser.add_argument('--buckets', type=int, dest='buckets',
                        help='buckets number in the backend', default=1)
    args = parser.parse_args()
    souffle_dir = args.souffle_dir
    slog_dir = args.slog_dir
    buckets = args.buckets
    for souffle_name in os.listdir(souffle_dir):
        # only take in facts
        print(f"importing {souffle_dir}/{souffle_name}")
        if souffle_name.endswith('.facts'):
            tsv_to_bin(f'{souffle_dir}/{souffle_name}', slog_dir, buckets)
