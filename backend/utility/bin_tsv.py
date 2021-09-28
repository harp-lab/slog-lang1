#!/usr/bin/env python

'''
a python scrpit convert a slog binary facts back into a csv

binary format:

;; 
;; Facts are represented in the frontend as tagged values that conform
;; to the tval? predicate. This representation allows ease of
;; debugging the interpreter. These facts are then organized into an
;; indices map as specified below.
;; 
;; Facts are represented in the backend as 64-bit tagged IDs
;; that encode either primitive values (possibly interned) or tuple
;; IDs. The format is represented as follows:
;; 
;; +-------------------+---------------------+--------------------+
;; | Rel tag (18 bits) | Bucket ID (18 bits) | Tuple ID (28 bits) |
;; +-------------------+---------------------+--------------------+
;; 
;; The tag is one of the following values:
;;  - 0 Integer 
;;  - 1 Float
;;  - 2 String
;;  - 3 Symbol
;;  - 4 Bool
;;  - N > 255. Relation IDs.
;;
;; Each relation is assigned an ID. Buckets and subbuckets are
;; compuated via a hash specified in the add-flat-fact
;; function. Tuples are assigned IDs via a "bump-pointer" allocation
;; strategy. In the interpreter, we count down from 2**28-1, as the
;; MPI backend counts up from 0 as it generates new tuples.

NOTE:
- file name must same as relation name
- `.size` file and other interning string file should also under meta_folder

Yihao Sun

'''

import os
import sys

HELP = "usage: bin_tsv <input> <arity> <output> <index> <meta_path>\n"

TAG_MASK = 0xFFFFC00000000000
BUCKET_MASK = 0x00003FFFF0000000
TUPLE_ID_MASK = 0xFFFFFFFFF0000000
VAL_MASK = ~ TAG_MASK

INT_TAG = 0
STRING_TAG = 2
SYMBOL_TAG = 3


def read_size_file(size_path):
    with open(size_path, "r") as size_file:
        return int(size_file.read())


def read_intern_file(fpath):
    ''' read intern file into a python dict '''
    intern_dict = {}
    with open(fpath, 'r') as intern_file:
        for line in intern_file.readlines():
            if line.strip() != '':
                splited = line.strip().split('\t')
                v_id = splited[0]
                if len(splited) == 1:
                    v = ''
                else:
                    v = splited[1]
                intern_dict[int(v_id)] = v
    return intern_dict


def bin_to_tsv(filename, arity, output, index, meta_folder):
    string_dict = read_intern_file(f"{meta_folder}/$strings.csv")
    symbol_dict = read_intern_file(f"{meta_folder}/$symbols.csv")
    rows = []
    with open(filename, "rb") as bin_file:
        bin_bytes = bin_file.read()
        print(f"binary file has {len(bin_bytes)/(8*(arity+1))} tuples")
        for i in range(0, len(bin_bytes), 8 * (arity + 1)):
            col_id = int.from_bytes(bin_bytes[i+arity*8:i+(arity+1)*8], 'little', signed=False)
            rel_tag = col_id >> 46
            bucket_hash = (col_id & BUCKET_MASK) >> 28
            tuple_id = col_id & TUPLE_ID_MASK
            print(f'tuple id -- ({rel_tag}, {bucket_hash}, {tuple_id})')
            attributes = [-1 for _ in range(arity)]
            for j in range(0, arity):
                raw_val = int.from_bytes(bin_bytes[i+j*8:i+(j+1)*8], 'little', signed=False)
                val_tag = raw_val >> 46
                if val_tag == INT_TAG:
                    attr_val = raw_val & VAL_MASK
                elif val_tag == STRING_TAG:
                    attr_val = string_dict[raw_val & VAL_MASK]
                elif val_tag == SYMBOL_TAG:
                    attr_val = symbol_dict[raw_val & VAL_MASK]
                else:
                    # relation
                    attr_val = f'rel_{raw_val & VAL_MASK}'
                attributes[index[j]-1] = attr_val
            rows.append(attributes)
    if os.path.exists(output):
        os.remove(output)
    with open(output, 'w+') as output_file:
        for line in rows:
            output_file.write('\t'.join(map(str, line)))
            output_file.write('\n')


if __name__ == '__main__':
    print("a python scrpit convert a slog binary facts back into a csv.")
    if len(sys.argv) != 6:
        print("argumments number wrong")
        print(HELP)
        exit(1)
    input_file_name = sys.argv[1]
    arity = int(sys.argv[2])
    output = sys.argv[3]
    raw_index = sys.argv[4]
    meta_folder = sys.argv[5]
    index = list(map(int, raw_index.strip().split(',')))
    bin_to_tsv(input_file_name, arity, output, index, meta_folder)
