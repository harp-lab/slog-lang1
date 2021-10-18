"""
a script add "" to all souffle symbol in fact file

"""

import argparse
import os

def add_quote_to_fact(souffle_fpath):
    quote_lines = []
    with open(souffle_fpath, 'r') as souffle_f:
        for line in souffle_f:
            line = line.strip()
            line_tmp = []
            if line == '':
                continue
            for col in line.split('\t'):
                if col.isnumeric():
                    line_tmp.append(col)
                elif col == '':
                    line_tmp.append('""')
                else:
                    line_tmp.append(f'"{col}"')
            quote_lines.append('\t'.join(line_tmp))
    with open(souffle_fpath, 'w') as souffle_f:
        souffle_f.write('\n'.join(quote_lines)+'\n')

def add_quote_to_folder(folder_path):
    if not os.path.isdir(folder_path):
        return
    for fname in os.listdir(folder_path):
        if not fname.endswith('.facts') or not fname.endswith('.csv'):
            continue
        add_quote_to_fact(os.path.join(folder_path, fname))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("souffle_fact_folder", help="Souffle fact folder")
    args = parser.parse_args()
    souffle_folder_fpath = os.path.realpath(args.souffle_fact_folder)
    add_quote_to_fact(souffle_folder_fpath)
