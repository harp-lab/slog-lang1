"""
Script convert souffle facts (tsv) into DeALS style facts

"""


from argparse import ArgumentParser
from glob import glob
import os


def add_quote(col: str):
    """ add quote if it is a souffle `symbol` """
    col = col.strip()
    if col.isdigit():
        return col
    return f"'{col}'"


def run(csv_dir, output):
    """ main entrance """
    with open(output, "w+") as output_f:
        for fact_path in glob(f"{csv_dir}/*.facts"):
            rel_name = os.path.basename(fact_path)[:-6]
            with open(fact_path, "r") as fact_f:
                for row in fact_f:
                    cols = list(map(add_quote, row.split('\t')))[:-1]
                    deals_fact = f"{rel_name}({','.join(cols)}).\n"
                    output_f.write(deals_fact)


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("csv_dir", help="input csv dir")
    parser.add_argument("output", help="output fac file")
    args = parser.parse_args()
    run(args.csv_dir, args.output)
