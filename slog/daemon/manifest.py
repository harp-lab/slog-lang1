"""
 Parsing manifest files

 Kris Micinski
 Yihao Sun
"""

import os
import pprint

import sexpdata
from sexpdata import Symbol


class MalformedManifest(Exception):
    """ manifest exception """


class Manifest():
    """
    order of Manifest info in data base:
    name,arity,select,"",data,size_file,tag
    becasue of history reason
    """

    def __init__(self, filename):
        self.relations = []
        with open(filename, 'r') as mf_file:
            sexpr = sexpdata.loads(mf_file.read())
            if sexpr[0] != Symbol('manifest'):
                raise MalformedManifest()
            relations = sexpr[1]
            if relations[0] != Symbol('relations'):
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
                tag = rel[3]
                bucket = rel[4]
                self.relations.append(
                    [name, arity, tag, bucket, select, data, size_file])
            self.string_file = sexpr[2]
            self.strings = {}

    def generate_mf(self, base_path):
        """ generate a new manifest file under some base directory """
        mf_s = Symbol('manifest')
        # count none empty lines in string
        relations_list = []
        for rel in self.relations:
            name = rel[0]
            arity = rel[1]
            tag = rel[2]
            bucket = rel[3]
            selection = rel[4]
            data_path = os.path.join(base_path, f'{name.value()}_{arity}')
            size_path = data_path + '.size'
            relations_list.append(
                [Symbol('relation'), name, arity, tag, bucket, selection, data_path, size_path])
        relations_s = [Symbol('relations'), relations_list]
        str_count = 0
        with open(os.path.join(base_path, '$strings.csv'), 'r') as string_f:
            for line in string_f.readlines():
                if line.strip() != '':
                    str_count = str_count + 1
        strings_s = [
            Symbol('strings'),
            '$strings.csv',
            str_count
        ]
        return sexpdata.dumps([mf_s, relations_s, strings_s])
