## Elaborator to transform and canonicalize instances of:
##   (include "../relative/path") 
## into
##   (include-hash "ba7816bf8...")
## where the file's contents have been hashed using sha256
import os
import re
import tempfile
import hashlib

class Elaborator():
    def __init__(self):
        self.include_pattern = re.compile('^include \"((?:[^\"]|\\\")*)\"$')
        self.hashes = {}
        self.path_to_hash = {}

    def calculate_preamble_length(self,path):
        length = 0
        with open(path) as f:
            for i, line in enumerate(f):
                if len(re.findall(self.include_pattern,line)) > 0:
                    length = i+1
        return length

    def traverse_dependencies(self,path):
        abs_path = os.path.abspath(path)
        if (abs_path in self.path_to_hash.keys()):
            return self.path_to_hash[path]
        preamble_length = self.calculate_preamble_length(abs_path)
        str = ""
        with open(abs_path) as f:
            for i, line in enumerate(f):
                if (i < preamble_length):
                    matches = re.findall(self.include_pattern,line)
                    if len(matches) > 0:
                        self.traverse_dependencies(os.path.join(os.path.dirname(abs_path),matches[0]))
                    str += "\n"
                else:
                    str += line
        h = hashlib.sha256()
        str = str.encode('utf-8')
        h.update(str)
        hsh = h.hexdigest()
        self.hashes[hsh] = str
        self.path_to_hash[abs_path] = hsh

    def elaborate(self,path):
        self.traverse_dependencies(path)
        print("Hashes:\n")
        for x in self.path_to_hash:
            print("{}\t{}".format(self.path_to_hash[x], x))
