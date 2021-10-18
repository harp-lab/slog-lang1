'''
compare a slog fact dir/file with a csv dir/file

Yihao Sun
'''

import argparse
import os

def get_csv_tuple_size(csv_path: str):
    """ get tuple number in a souffle style fact file """
    with open(csv_path, 'r') as csv_f:
        csv_tuple_size = len(csv_f.readlines()) - 1
    return csv_tuple_size

def get_slog_table_tuple_size(slog_table_path):
    arity = int(slog_table_path.split('.')[-2])
    slog_tuple_size = os.path.getsize(slog_table_path) / (8 * (arity + 1))
    return slog_tuple_size

def compare_file_size(slog_table_path: str, csv_path: str):
    """ compare the size of a slog table file with a souffle csv file"""
    # get arity from slog table naming rule
    csv_tuple_size = get_csv_tuple_size(csv_path)
    slog_tuple_size = get_slog_table_tuple_size(slog_table_path)
    if csv_tuple_size != slog_tuple_size:
        print(f"{slog_table_path} contain different size!")
        print(f"slog database contain {slog_tuple_size} facts, but...")
        print(f"csv folder contain {csv_tuple_size}.")
    return csv_tuple_size == slog_tuple_size

def compare_folder_size(slog_folder, csv_folder):
    if not os.path.isdir(slog_folder) or not os.path.isdir(csv_folder):
        print("input is not a dir")
        return
    res = True
    csv_fnames = os.listdir(csv_folder)
    for slog_name in os.listdir(slog_folder):
        base_name = slog_name[:-len(".table")]
        rel_name = base_name[base_name.find('.'):base_name.rfind('.')]
        if f'{rel_name}.csv' in csv_fnames:
            res = res and compare_file_size(os.path.join(slog_folder, slog_name),
                                            os.path.join(csv_folder, f'{rel_name}.csv'))
        elif f'{rel_name}.facts' in csv_fnames:
            res = res and compare_file_size(os.path.join(slog_folder, slog_name),
                                            os.path.join(csv_folder, f'{rel_name}.facts'))
    return res

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("slog_folder", help="Slog database folder")
    parser.add_argument("csv_folder", help= "CSV folder")
    args = parser.parse_args()
    slog_folder = os.path.realpath(args.slog_folder)
    csv_folder = os.path.realpath(args.csv_folder)
    compare_file_size(slog_folder, csv_folder)
