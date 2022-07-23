'''
Load and represent a relation efficiently in Python
Kris Micinski, 2022
'''
from unittest import loader
import numpy as np

import slog.protobufs.slog_pb2 as slog_pb2
from slog.common.tuple import *

MAX_RELATION_TUP_SIZE = 100000

class RelationTooLargeExn(Exception):
    pass

class GrpcRelationLoader:
    def __init__(self,_stub):
        self._stub = _stub

    def load_relation_data(self,cached_rel):
        tupsize = cached_rel.arity + 1
        if (cached_rel.num_tuples > MAX_RELATION_TUP_SIZE):
            raise RelationTooLargeExn()
        req = slog_pb2.RelationRequest()
        req.database_id = cached_rel.dbid
        req.tag = cached_rel.tag
        total_tups = 0
        batches = []
        for response in self._stub.GetTuples(req):
            total_tups = total_tups + response.num_tuples
            batches.append((np.array(response.data),response.num_tuples))
        # Total tuples and number of tuples should match
        assert(total_tups == cached_rel.num_tuples)
        # Create large array
        cached_rel.tuple_data = np.empty(shape=(total_tups,tupsize),dtype=np.uint64)
        offset = 0
        for batch in batches:
            batch_data = batch[0]
            batch_tups = batch[1]
            for tup in batch_data.reshape(batch_tups,tupsize):
                print(tup)
                tup_id = tup[0]
                tup_num = tup[0] & (~TUPLE_ID_MASK)
                cached_rel.tuple_data[tup_num] = tup
        # Indicate now loaded
        print(cached_rel.tuple_data[tup_num])
        self.loaded = True

class CachedRelation:
    def __init__(self,dbid:str,name:str,arity,tag,num_tuples,loader:GrpcRelationLoader):
        self.dbid = dbid
        self.name = name
        self.arity = arity
        self.tag = tag
        self.num_tuples = num_tuples
        self.loaded = False
        self.tuple_data = None
        self.loader = loader

    def too_large(self): return self.num_tuples > MAX_RELATION_TUP_SIZE

    def load(self):
        if self.too_large():
            raise RelationTooLargeExn()
        if self.loaded: return
        self.loader.load_relation_data(self)

    def get_tuple(self,tuple_id) -> list[np.uint64]:
        """Retrieve a particular tuple, """
        if self.too_large():
            raise RelationTooLargeExn()
        if (not self.loaded):
            self.load()
        assert(self.num_tuples > tuple_id)
        tuplen = self.arity + 1
        return self.tuple_data[(tuplen*tuple_id):(tuplen*tuple_id+tuplen)]
    
