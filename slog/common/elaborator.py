'''
Preprocesser of slog code

TODO: make this class hashable, so it can be used in cache

'''

import os
import re
import hashlib

class Elaborator():
    '''
    Elaborator to transform and canonicalize instances of:
    (include "../relative/path")
    into
    (include-hash "ba7816bf8...")
    where the file's contents have been hashed using sha256
    '''
    def __init__(self):
        self.include_pattern = re.compile('^(include \"((?:[^\"]|\\\")*)\")$')
        self.hashes = {}
        self.path_to_hash = {}

    def calculate_preamble_length(self, path):
        length = 0
        with open(path) as f:
            for i, line in enumerate(f):
                if len(re.findall(self.include_pattern, line)) > 0:
                    length = i+1
        return length

    def traverse_dependencies(self, path):
        abs_path = os.path.abspath(path)
        if abs_path in self.path_to_hash.keys():
            return self.path_to_hash[path]
        preamble_length = self.calculate_preamble_length(abs_path)
        elaborated_code = ""
        with open(abs_path) as slog_file:
            for i, line in enumerate(slog_file):
                if i < preamble_length:
                    matches = re.findall(self.include_pattern, line)
                    if len(matches) > 0:
                        self.traverse_dependencies(os.path.join(os.path.dirname(abs_path),
                                                   matches[0]))
                    elaborated_code += "\n"
                else:
                    elaborated_code += line
        hash_func = hashlib.sha256()
        elaborated_code = elaborated_code.encode('utf-8')
        hash_func.update(elaborated_code)
        hsh = hash_func.hexdigest()
        self.hashes[hsh] = elaborated_code
        self.path_to_hash[abs_path] = hsh

    def elaborate(self, path):
        """ preprocess a slog file do include things """
        self.traverse_dependencies(path)
