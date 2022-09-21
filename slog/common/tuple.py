'''
Data format convert untility function

Yihao Sun
'''


from os import lstat
from platform import release
import string
from slog.common.dbcache import *
import numpy

TAG_MASK =      0xFFFFC00000000000
BUCKET_MASK =   0x00003FFFF0000000
TUPLE_ID_MASK = 0xFFFFFFFFF0000000
U32_MASK =      0x00000000FFFFFFFF
VAL_MASK = ~ TAG_MASK
TAG_SHIFT = numpy.uint(46)
INT_TAG = 0
STRING_TAG = 2
SYMBOL_TAG = 3

## New S-Expr API
class SExprVisitor:
    """Visitor API that allows walking over s-expressions"""
    def __init__(self): pass
    def visitBuiltinNumber(self,number): pass
    def visitBuiltinSymbol(self,string): pass
    def visitBuiltinString(self,string): pass
    def initVisitLoadedTuple(self,relation,tuple_id): pass
    def nextVisitLoadedTuple(self,relation,n): pass
    def exitVisitLoadedTuple(self,relation,tuple_id): pass
    def visitUnloadedTuple(self,relation,tuple_id): pass

class CachedStructuredData:
    """
    Abstract base class
    """
    def __init__(self):
        pass
    
    def visit(self,visitor): pass

class BuiltinNumber(CachedStructuredData):
    def __init__(self,num):
        self.value = num

    def visit(self,visitor): visitor.visitBuiltinNumber(self.value)

class BuiltinString(CachedStructuredData):
    def __init__(self,str:str):
        self.value = str

    def visit(self,visitor): visitor.visitBuiltinNumber(self.value)

class LoadedTuple(CachedStructuredData):
    """
    A tuple with some number of elements
    """
    def __init__(self,relation,tuple_id,data:list[CachedStructuredData]):
        self.relation = relation
        self.children = data
        self.tuple_id = tuple_id

    def visit(self,visitor:SExprVisitor):
        visitor.initVisitLoadedTuple(self.relation,self.tuple_id)
        n = 1
        for d in self.children:
            visitor.nextVisitLoadedTuple(self.relation,n)
            n = n+1
            d.visit(visitor)
        visitor.exitVisitLoadedTuple(self.relation,self.tuple_id)

class UnloadedTuple(CachedStructuredData):
    """An unloaded tuple"""
    def __init__(self,relation,id):
        self.relation = relation
        self.id = id

    def visit(self,visitor:SExprVisitor):
        visitor.visitUnloadedTuple(self.relation,self.id)

class TupleHistory:
    """
    Tracks state relevant to deduplicating tuples when in an interactive session

    When interactively rendering tuples (e.g., in a REPL or debugging output), 
    it is often preferable to deduplicate facts, to support recognizing that:
        (a (b 1 2) (c)) and (a (b 1 2) (d))
    share an overlapping (b 1 2)^1. In such circumstances, we often wish to 
    handle this in some way (e.g.,abbreviating an expression as a variable).
    """
    def __init__(self):
        # Maps a database (ID, str) to a dict of tupids to counts
        self.tuple_counts = {}
        self.names = {}
        self.ids = 0

    def bump_count(self,db_id:str,tuple_id:numpy.uint64):
        """ Bumps a tuple ID's count--call this each time a tuple ID is seen. """
        if (not (db_id in self.tuple_counts)):
            new_db = {}
            self.tuple_counts[db_id] = new_db
        if (not (tuple_id in self.tuple_counts[db_id])):
            self.tuple_counts[db_id][tuple_id] = 1
            return 1
        else:
            curct = self.tuple_counts[db_id][tuple_id]
            self.tuple_counts[db_id][tuple_id] = curct + 1
            return curct + 1
    
    def get_count(self,db_id:str,tuple_id:numpy.uint64):
        return self.tuple_counts[db_id][tuple_id]

    def new_name(self):
        r = f'x{self.ids}'
        self.ids = self.ids + 1
        return r

    def get_name(self,db_id:str,tuple_id,force=False):
        """ Get a tuple's name; if force is True a new name will be generated
            (unless one exists) no matter what. """
        return None
        # Add a new name when > 1 occurrence
        if self.get_count(db_id,tuple_id) > 1 or force:
            if tuple_id in self.names:
                return self.names[tuple_id]
            else:
                name = self.new_name()
                self.names[tuple_id] = name
                return name
        else: return None

class TupleLoader:
    def __init__(self,database,relation,starting_offset,history):
        self.db = database
        self.cur_tuple_id = starting_offset
        self.relation = relation
        self.history = history

    def parse_tuple_row(self, u64_list) -> CachedStructuredData:
        """ parse a row of u64 tuple into a python object """
 
    def _materialize_tuple(self,tuple_id:numpy.uint64,relation,depth,history:TupleHistory) -> CachedStructuredData:
        """
        Materialize an approximation of particular tuple to some specific depth
        """
        if depth == 0:
            return UnloadedTuple(relation,tuple_id)
        raw_tuple = relation.get_tuple(tuple_id)
        # Visit the tuple via the history
        history.bump_count(self.db.dbid,tuple_id)
        # Build the children
        children = []
        # Iterate over rest of columns
        for u64 in raw_tuple[1:]:
            val_tag = u64 >> TAG_SHIFT
            # Integer
            if val_tag == INT_TAG:
                attr_val = (u64 & VAL_MASK)
                if attr_val > 2 ** 31:
                    attr_val = attr_val - 2 ** 32
                children.append(BuiltinNumber(attr_val))
            # String
            elif val_tag == STRING_TAG:
                children.append(BuiltinString(self.db.lookup_string(u64 & numpy.uint64(U32_MASK))))
            # Relation
            else:
                rel_tag = u64 >> TAG_SHIFT
                newrel = self.db.relation_by_tag(rel_tag)
                children.append(self._materialize_tuple(u64,newrel,depth-1,history))
        return LoadedTuple(relation,tuple_id,children)

    def tuples_left(self):
        """Returns the number of tuples left"""
        return self.relation.num_tuples - self.cur_tuple_id

    def materialize_tuples(self,n,depth) -> CachedStructuredData:
        """
        Materialize the next n tuples.
        """
        ending_offset = self.cur_tuple_id + n
        if (self.cur_tuple_id < self.relation.num_tuples or self.cur_tuple_id < 0):
            pass
        else:
            raise ValueError(f'Index {n} wrong large for relation {self.relation.name} ({self.relation.arity}) which has only {self.relation.num_tuples} tuples')
        lst = []
        while (self.cur_tuple_id < ending_offset):
            lst.append(self._materialize_tuple(self.cur_tuple_id,self.relation,depth,self.history))
            self.cur_tuple_id += 1
        return lst
