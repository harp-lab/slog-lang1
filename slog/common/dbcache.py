'''
Database cache
Kris Micinski, 2022
'''

from slog.common.relation import CachedRelation, GrpcRelationLoader
import slog.protobufs.slog_pb2 as slog_pb2

class CachedDatabase:
    def __init__(self,dbid,relations):
        self.dbid = dbid
        self.relations = {}
        for relation in relations:
            self.relations[relation.tag] = relation
            self.relations[relation.name] = relation

    def relation_by_tag(self,tag): return self.relations[tag]
    def relation_by_name(self,name): return self.relations[name]

class DatabaseLoader: 
    def load_database(self,dbid) -> CachedDatabase:
        pass

class GrcpDatabaseLoader:
    def __init__(self,_stub):
        self._stub = _stub
        self.loader = GrpcRelationLoader(self._stub)

    def load_database(self,dbid) -> CachedDatabase:
        """Load a new DB"""
        req = slog_pb2.DatabaseRequest()
        req.database_id = dbid
        res = self._stub.GetRelations(req)
        rels = list(map(lambda rel: CachedRelation(dbid,rel.name,rel.arity,rel.tag,rel.num_tuples,self.loader)
                        , res.relations))
        db = CachedDatabase(dbid,rels)
        return db
    
class DatabaseCache:
    """
    an abstraction around a database cache; we will load tables lazily
    from a given database, this class facilitates that
    """
    def __init__(self, dbloader: DatabaseLoader):
        self.databases = {}
        self.loader = dbloader

    def database(self,dbid):
        """lazily loads a database unless it exists already"""
        if (dbid in self.databases):
            return self.databases[dbid]
        else:
            self.databases[dbid] = self.loader.load_database(dbid)
            return self.databases[dbid]

    def reset(self):
        """Called when a client does something like reconnect"""
        self.databases = {}
