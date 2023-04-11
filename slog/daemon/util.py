"""
Some util function

Kris Micinski
Yihao Sun
"""

import os
import re
import hashlib

import numpy as np

# STRING_FNV_PRIME = np.uint32(16777619)
# STRING_FNV_BASE_OFFSET = np.uint32(2166136261)
STRING_FNV_PRIME = np.uint64(1099511628211)
STRING_FNV_BASE_OFFSET = np.uint64(14695981039346656037)
U46MAX = np.uint64(35184372088832)

def join_hashes(hashes):
    """ join 2 hash values? """
    return ",".join(hashes)


def string_hash(target_str: str):
    """ compute the FNV hash value of string (same as backend string) """
    hsh = STRING_FNV_BASE_OFFSET
    for _c in target_str:
        hsh ^= np.uint64(ord(_c))
        hsh = hsh * STRING_FNV_PRIME
    return hsh % U46MAX


def generate_db_hash(hashes, using_db=None):
    """
    calculate an output database hash from a combined hash of a list of
    hashes of input files and previous db
    using_db == "" if not using any other DB
    """
    if using_db != "" and using_db is not None:
        hashes = [using_db] + hashes
    hashes.sort()
    cathashes = "|".join(hashes).encode('utf-8')
    hash_func = hashlib.sha256()
    hash_func.update(cathashes)
    return hash_func.hexdigest()


def compute_hash_file(fdata):
    """ generate a hash value from a file content data """
    hash_func = hashlib.sha256()
    hash_func.update(fdata)
    hsh = hash_func.hexdigest()
    return hsh


def read_intern_file(fpath):
    ''' read intern file into a python dict '''
    intern_dict = {}
    with open(fpath, 'r') as intern_file:
        for line in intern_file.readlines():
            if line.strip() != '':
                splited = line.strip().split('\t')
                v_id = splited[0]
                if len(splited) == 1:
                    str_val = ''
                else:
                    str_val = splited[1].strip()
                intern_dict[int(v_id)] = str_val
    return intern_dict


def split_hashes(hashes):
    """ split 2 hash values? """
    return hashes.split(",")


def rel_name_covert(rel_name: str):
    """ convert a relation name in code into name in output """
    return rel_name.replace('_', '__').replace('.', '_dot')


def checkpoint_ord(check_dir):
    """ order of checkpoint folder """
    check_stamp = re.findall(r'checkpoint-(\d+)-(\d+)', check_dir)
    if len(check_stamp) == 0:
        return [0, 0]
    else:
        return [int(check_stamp[0][1]), int(check_stamp[0][0])]


def compute_relation_row_size(datafile, arity):
    """ compute the row size of a relation datafile """
    return int(os.path.getsize(datafile) / (8 * (arity + 1)))


def get_relation_info(datapath):
    """ get the basic infomation of a relation form it's path """
    fname = os.path.basename(datapath)
    if not fname.endswith('.table') and not fname.endswith('.table_full'):
        print(f"Invalid relname {fname}")
        return False
    else:
        fname = fname[:fname.rfind('.')]
    tag_s = fname.split('.')[0]
    arity_s = fname.split('.')[-1]
    rel_name = fname[len(tag_s)+1:-len(arity_s)-1]
    return {
        "name": rel_name,
        "arity": int(arity_s),
        "tag": int(tag_s),
        "num_tuples": compute_relation_row_size(datapath, int(arity_s)),
        "data_file": datapath
    }
