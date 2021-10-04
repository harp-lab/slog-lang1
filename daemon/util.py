"""
Some util function

Kris Micinski
Yihao Sun
"""

import re
import hashlib


def join_hashes(hashes):
    """ join 2 hash values? """
    return ",".join(hashes)


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
