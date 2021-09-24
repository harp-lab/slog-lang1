# 
# Parsing manifest files
#
from sexpdata import Symbol
import sexpdata

class MalformedManifest(Exception):
    pass

class Manifest():
    """
    order of Manifest info in data base:
    name,arity,select,"",data,size_file,tag
    becasue of history reason
    """
    def __init__(self,filename):
        self.relations = []
        with open(filename,'r') as f:
            sexpr = sexpdata.loads(f.read())
            if (sexpr[0] != Symbol('manifest')):
                raise MalformedManifest()
            relations = sexpr[1]
            if (relations[0] != Symbol('relations')):
                raise MalformedManifest()
            per_rel = relations[1]
            for rel in per_rel:
                if rel[0] != Symbol('relation'):
                    raise MalformedManifest()
                name = rel[1]
                arity = rel[2]
                select = rel[5]
                data = rel[6]
                size_file = rel[7]
                tag  = rel[4]
                self.relations.append([name,arity,select,"",data,size_file,tag])
            # string_pool = sexpr[2][1]
            self.string_file = sexpr[2]
            self.strings = {}

