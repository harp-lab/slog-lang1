# 
# Parsing manifest files
#
from sexpdata import Symbol
import sexpdata

class MalformedManifest(Exception):
    pass

class Manifest():
    def __init__(self,filename):
        self.relations = []
        with open(filename,'r') as f:
            sexpr = sexpdata.loads(f.read())
            if (sexpr[0] != Symbol('manifest')):
                raise MalformedManifest()
            directories = sexpr[1]
            if (directories[0] != Symbol('directories')):
                raise MalformedManifest()
            per_rel = directories[1]
            for rel in per_rel:
                if (rel[0] != Symbol('rel-select-file')):
                    raise MalformedManifest()
                canonical = (rel[1] == Symbol('canonical'))
                name = rel[2]
                arity = rel[3]
                select = rel[4]
                data = rel[5]
                size_file = rel[6]
                tag  = rel[7]
                self.relations.append([name,arity,select,canonical,data,size_file,tag])
            string_pool = sexpr[2][1]
            self.strings = {}
            for entry in string_pool:
                self.strings[entry[0]] = entry[1]

