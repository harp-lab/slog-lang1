'''
Data format convert untility function

Yihao Sun
'''


from os import lstat
from platform import release
from slog.common.dbcache import *
import numpy

TAG_MASK =      0xFFFFC00000000000
BUCKET_MASK =   0x00003FFFF0000000
TUPLE_ID_MASK = 0xFFFFFFFFF0000000
U32_MASK =      0x00000000FFFFFFFF
VAL_MASK = ~ TAG_MASK
INT_TAG = 0
STRING_TAG = 2
SYMBOL_TAG = 3

class SlogTuple:
    """
    a python data class to represent a slog tuple
    a nest relation coloumn will be `(NESTED ...)`
    """
    def __init__(self, rel_name, col):
        self.rel_name = rel_name
        self.print_name = None
        self.col = col
        self.tuple_id = col[0]
        self.data_col = col[1:]
        self.tag = col[0][0]
        self.arity = len(col) - 1

    def __eq__(self, o: object) -> bool:
        return self.tuple_id == o.tuple_id

    def __str__(self) -> str:
        return str(self.col)

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

class LoadedTuple(CachedStructuredData):
    """
    A tuple with some number of elements
    """
    def __init__(self,relatio,tuple_id,data:list[CachedStructuredData]):
        self.relation = relation
        self.data = data
        self.tuple_id = tuple_id

    def visit(self,visitor):
        visitor.initVisitLoadedTuple(self.tuple_id)
        for d in self.data:
            d.visit(self)
        visitor.exitVisitLoadedTuple(self.tuple_id)

class UnloadedTuple(CachedStructuredData):
    """An unloaded tuple"""
    def __init__(self,relation,id):
        self.relation = relation
        self.id = id

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
            new_db[tuple_id] = 1
            self.tuple_counts[db_id] = new_db
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
    def __init__(self,relation,starting_offset,history):
        self.cur_tuple_id = starting_offset
        self.relation = relation
        pass

    def _materialize_tuple(self,tuple_id,depth,history) -> CachedStructuredData:
        """
        Materialize an approximation of particular tuple to some specific depth
        """
        return

    def tuples_left(self):
        """Returns the number of tuples left"""
        return relation.num_tuples - self.cur_tuple_id

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
            lst.append(self._materialize_tuple(self.cur_tuple_id,depth,self.history))
            self.cur_tuple_id += 1


class TupleParser:
    """
    A parser will parse a slog query result u64 tuple set into well-formated string.
    """
    def __init__(self):
        pass

    def parse_tuple_row(self, u64_list, intern_string_dict) -> SlogTuple:
        """ parse a row of u64 tuple into a python object """
        parsed_row = []
        # index col
        rel_tag = u64_list[0] >> 46
        bucket_id = (u64_list[0] & BUCKET_MASK) >> 28
        tuple_id = u64_list[0] & (~TUPLE_ID_MASK)
        t_id = (rel_tag, bucket_id, tuple_id)
        parsed_row.append(t_id)
        if t_id in self.count_map:
            self.count_map[t_id] += 1
        else:
            self.count_map[t_id] = 0
        for u64 in u64_list[1:]:
            val_tag = u64 >> 46
            if val_tag == INT_TAG:
                attr_val = (u64 & VAL_MASK)
                if attr_val > 2 ** 31:
                    attr_val = attr_val - 2 ** 32
            elif val_tag == STRING_TAG:
                attr_val = intern_string_dict[u64 & U32_MASK]
            else:
                # relation
                bucket_id = (u64 & BUCKET_MASK) >> 28
                tuple_id = u64 & (~TUPLE_ID_MASK)
                attr_val = ('NESTED', val_tag, bucket_id, tuple_id)
            parsed_row.append(attr_val)


    def tuple_to_str(self, slog_tuple: SlogTuple, cur_max_depth):
        """ turn a python tuple  into sexpr string """
        # print(rel)
        if self.count_map[slog_tuple.tuple_id] >= self.cardinality:
            printed_id = None
            for p_id, _t in self.printed_id_map.items():
                if _t == slog_tuple:
                    printed_id = p_id
            return f"#{printed_id}"
        res = []
        rel_name = slog_tuple.rel_name
        for col in slog_tuple.data_col:
            if isinstance(col, tuple):
                if col[0] == 'NESTED':
                    nested_tag = col[1]
                    val = None
                    printed_id, val = self.reversed_id_map[col[1:]]
                    if val is None:
                        nested_id = col[3]
                        res.append(f'"{self.tag_map[nested_tag]} has no fact with id {nested_id} !"')
                    elif cur_max_depth == 0:
                        res.append(f"({self.tag_map[nested_tag][0]} #{printed_id})")
                    else:
                        res.append(self.tuple_to_str(val, cur_max_depth-1))
            else:
                res.append(str(col))
        return f"({rel_name} {' '.join(res)})"

    def pretty_str_tuples(self, print_rel_list):
        """ pretty stringfy a list of relation """
        res = []
        rel_tag_list = [r[2] for r in print_rel_list]
        # for slog_tuple in slog_tuples:
        #     self.count_tuples(slog_tuple, self.max_depth, self.printed_id_map)
        unfolded_ids = []
        for t_id, count in self.count_map.items():
            if count == 0 or count >= self.cardinality:
                unfolded_ids.append(t_id)
        for p_id, val in self.printed_id_map.items():
            if val.tuple_id[0] in rel_tag_list and val.tuple_id in unfolded_ids:
                pp_str = self.tuple_to_str(val, self.max_depth)
                res.append(f"#{p_id}\t{pp_str}")
        return res

    def pretty_str_printed_id(self, printed_id):
        """ pretty print an slog tuple with given printed_id """
        pp_str = self.tuple_to_str(self.printed_id_map[printed_id], self.max_depth)
        return f"#{printed_id}\t{pp_str}"

    def parse_query_result(self):
        """
        parse a query result (u64) into python slog tuples object
        query_res is a mapping of relation tag to list of u64 tuples.
        also a tag to (name,arity) mapping is required.
        a intern string mapping is required to render string.
        if nested relation reach max_nested_depth, it will printed as
        `({rel name} #{row count})`. If a fact appears more than
        `cardinality` times, it won't be nested

        return a list of sexpr facts (if some other relation nested,
        they will also been there) and the updated print id map

        NOTE:
        in printed map only index > 0 index will be printed
        This function will update printed_id_map
        """
        parsed_tuples = []

        # parse all u64 tuples to python list first
        for rel_tag, tuples in self.query_res.items():
            if tuples == []:
                continue
            rel_name = self.tag_map[rel_tag][0]
            rel_arity = self.tag_map[rel_tag][1]
            buf = []
            for u64 in tuples:
                buf.append(u64)
                if len(buf) == rel_arity + 1:
                    slog_tuple = self.parse_tuple_row(buf, rel_name, self.intern_string_dict)
                    parsed_tuples.append(slog_tuple)
                    buf = []
        if self.printed_id_map == {}:
            cur_printed_id = 0
        else:
            cur_printed_id = max(self.printed_id_map.keys())
        #  update tuple tag map
        # generate a reversed map
        # reversed_id_map = dict([reversed(i.tuple_id) for i in self.printed_id_map.items()])
        for new_tuple in parsed_tuples:
            # p_id = find_printed_id_by_val(printed_id_map, new_tuple)
            if new_tuple.tuple_id not in self.reversed_id_map:
                cur_printed_id += 1
                self.printed_id_map[cur_printed_id] = new_tuple
                self.reversed_id_map[new_tuple.tuple_id] = (cur_printed_id, new_tuple)
        return parsed_tuples
