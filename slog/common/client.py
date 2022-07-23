"""
Driver of the REPL; these are common 'verbs' for things to do


"""

from functools import lru_cache
from ftplib import FTP
import hashlib
import os
import sys
import time
from typing import Tuple

import grpc
from prompt_toolkit import PromptSession
from six import MAXSIZE
import psutil
from slog.common import rel_name_from_file, make_stub, PING_INTERVAL
from slog.common.dbcache import *
from slog.common.elaborator import Elaborator
from slog.common.relation import GrpcRelationLoader
from slog.common.tuple import CachedStructuredData, SExprVisitor, TupleHistory, TupleLoader, TupleParser
from slog.daemon.const import FTP_DEFAULT_PWD, FTP_DEFAULT_USER, STATUS_PENDING, STATUS_FAILED, STATUS_RESOLVED, STATUS_NOSUCHPROMISE
from slog.daemon.util import get_relation_info
import slog.protobufs.slog_pb2 as slog_pb2

CSV_CHUNK_SIZE = 1024

def digest_file(fpath):
    """ md5 digest of a large file """
    hash_func = hashlib.md5()
    with open(fpath, 'rb') as large_f:
        while True:
            chunk = large_f.read(2**20)
            if not chunk:
                break
            hash_func.update(chunk)
    return hash_func.hexdigest()

class Writer:
    """
    Does nothing when given a write command.
    """
    def write(self, _):
        ...

    def ok(self, _):
        ...

    def fail(self, _):
        ...

    def __eq__(self, o: object) -> bool:
        return True

    def __hash__(self) -> int:
        return 0

class ConsoleWriter(Writer):
    """
    Simple class to print when given a write command
    """
    def write(self, out):
        print(out)

    def ok(self, out):
        print(out)

    def fail(self, out):
        print(out)

class FileWriter(Writer):
    def __init__(self, f):
        self.f = f
    def write(self, out):
        self.f.write(f'{out}\n')
    def ok(self, out):
        self.f.write(f'{out}\n')
    def fail(self, out):
        self.f.write(f'{out}\n')

    def __eq__(self, o: object) -> bool:
        return self.f.name == o.name

    def __hash__(self) -> int:
        return hash(self.f.name)

class TuplePrettyPrinter(SExprVisitor):
    """ Uses a visitor to dump an S-Expression to raw text """
    def __init__(self,history:TupleHistory):
        super().__init__()
        self.history = history
        self.str_pieces = ""
        self.depth = 0
        self.subscript_map = {
            "0": "₀", "1": "₁", "2": "₂", "3": "₃", "4": "₄", "5": "₅", "6": "₆",
            "7": "₇", "8": "₈", "9": "₉", "a": "ₐ", "b": "♭", "c": "꜀", "d": "ᑯ",
            "e": "ₑ", "f": "բ", "g": "₉", "h": "ₕ", "i": "ᵢ", "j": "ⱼ", "k": "ₖ",
            "l": "ₗ", "m": "ₘ", "n": "ₙ", "o": "ₒ", "p": "ₚ", "q": "૧", "r": "ᵣ",
            "s": "ₛ", "t": "ₜ", "u": "ᵤ", "v": "ᵥ", "w": "w", "x": "ₓ", "y": "ᵧ",
            "z": "₂", "A": "ₐ", "B": "₈", "C": "C", "D": "D", "E": "ₑ", "F": "բ",
            "G": "G", "H": "ₕ", "I": "ᵢ", "J": "ⱼ", "K": "ₖ", "L": "ₗ", "M": "ₘ",
            "N": "ₙ", "O": "ₒ", "P": "ₚ", "Q": "Q", "R": "ᵣ", "S": "ₛ", "T": "ₜ",
            "U": "ᵤ", "V": "ᵥ", "W": "w", "X": "ₓ", "Y": "ᵧ", "Z": "Z", "+": "₊",
            "-": "₋", "=": "₌", "(": "₍", ")": "₎"
        }
        self.superscript_map = {
            "0": "⁰", "1": "¹", "2": "²", "3": "³", "4": "⁴", "5": "⁵", "6": "⁶",
            "7": "⁷", "8": "⁸", "9": "⁹", "a": "ᵃ", "b": "ᵇ", "c": "ᶜ", "d": "ᵈ",
            "e": "ᵉ", "f": "ᶠ", "g": "ᵍ", "h": "ʰ", "i": "ᶦ", "j": "ʲ", "k": "ᵏ",
            "l": "ˡ", "m": "ᵐ", "n": "ⁿ", "o": "ᵒ", "p": "ᵖ", "q": "۹", "r": "ʳ",
            "s": "ˢ", "t": "ᵗ", "u": "ᵘ", "v": "ᵛ", "w": "ʷ", "x": "ˣ", "y": "ʸ",
            "z": "ᶻ", "A": "ᴬ", "B": "ᴮ", "C": "ᶜ", "D": "ᴰ", "E": "ᴱ", "F": "ᶠ",
            "G": "ᴳ", "H": "ᴴ", "I": "ᴵ", "J": "ᴶ", "K": "ᴷ", "L": "ᴸ", "M": "ᴹ",
            "N": "ᴺ", "O": "ᴼ", "P": "ᴾ", "Q": "Q", "R": "ᴿ", "S": "ˢ", "T": "ᵀ",
            "U": "ᵁ", "V": "ⱽ", "W": "ᵂ", "X": "ˣ", "Y": "ʸ", "Z": "ᶻ", "+": "⁺",
            "-": "⁻", "=": "⁼", "(": "⁽", ")": "⁾"
        }

    def subscript(self,txt):
        sub_trans = str.maketrans(
        ''.join(self.subscript_map.keys()),
        ''.join(self.suberscript_map.values()))
        return txt.translate(sub_trans)

    def superscript(self,txt):
        sup_trans = str.maketrans(
        ''.join(self.superscript_map.keys()),
        ''.join(self.superscript_map.values()))
        return txt.translate(sup_trans)

    def pretty_print_tuple(self,tuple:CachedStructuredData):
        self.depth = 0
        tuple.visit(self)
        self.dump()

    def append(self,str):
        self.str_pieces += str
        return self

    def dump(self):
        final = "".join(self.str_pieces)
        self.str_pieces = []

    def visitBuiltinNumber(self, number):
        self.append(str(number))
    def visitBuiltinString(self, s):
        self.append(s)
    def visitBuiltinSymbol(self, s):
        self.append(s)
    def initVisitLoadedTuple(self, relation, tuple_id):
        self.append("(").append(relation.name)
        self.depth += 1
    def nextVisitLoadedTuple(self, relation: CachedRelation, n):
        self.append(" ")
    def exitVisitLoadedTuple(self, relation: CachedRelation, tuple_id):
        self.append(")")
        self.depth += 1
        # If tuple has a name that we should print, print it after in superscript
        if (self.history.get_name(tuple_id) != None):
            self.append(self.superscript(self.history.get_name(tuple_id)))
    def visitUnloadedTuple(self, relation: CachedRelation, tuple_id):
        self.append("...").append(self.superscript(self.history.get_name(relation.dbid,tuple_id,True)))

class TuplePaginator:
    """
    Load and print tuples in pages
    """
    def __init__(self,writer,relation,history,prompt_session:PromptSession,batch_size = 5):
        self.writer = writer
        self.loader = TupleLoader(relation,0,history)
        self.batch_size = batch_size
        self.printer = TuplePrettyPrinter(history)
        self.prompt_session = prompt_session

    def print_all_tuples(self,relation):
        while self.loader.tuples_left() > 0:
            self.print_next_batch()

    def print_next_batch(self,relation:CachedRelation):
        grab = min(self.loader.tuples_left(),self.batch_size)
        tuples = self.loader.materialize_tuples(grab)
        for tuple in tuples:
            self.printer.pretty_print_tuple(tuple)

    def interactively_print_all(self,relation):
        while self.loader.tuples_left() > 0:
            self.print_next_batch()
            answer = self.prompt_session.confirm(f"Load more facts? (seen {self.loader.cur_tuple_id} of {self.loader.relation.num_tuples})")
            if (not answer): return

class SlogClient:
    """
    Client to a slog server.
    """
    def __init__(self, server="localhost", rpc_port=5108, ftp_port=2121, local_db_path=None):
        self._channel = None
        self._stub = None
        self.server_addr = server
        self.ftp_port = ftp_port
        self.db_cache = None

        # XXX: Refactor local path stuff ASAP
        self.local_db_path = local_db_path
        #if not local_db_path:

        try:
            self.connect(f"{server}:{rpc_port}")
        except grpc.RpcError:
            print(f"Can't connect to slog server on {server}:{rpc_port}")
            sys.exit(1)

        # XXX
        self.lasterr = None
        self.relations = [] #DatabaseRelationsCache()
        self.cur_db = None
        self._cur_program_hashes = None
        self.intern_string_dict = {}
        self.updated_tuples = {}
        self.all_db = []
        self.history = TupleHistory()

        # map of tuple with it's print name
        self.tuple_printed_id_map = {}
        self.slog_tuple_parser = None

        if local_db_path:
            self.load_relations(None)
            self._update_intern_strings(None)

    def connect(self, server):
        """ Reconnect to the rpc server """
        self.server = server
        self._channel, self._stub = make_stub(server)
        self._cur_time = -1
        # Hash from timestamps to database hashes
        self.loader = GrcpDatabaseLoader(self._stub)
        self.db_cache = DatabaseCache(self.loader)
        self._times = {}
        self.db_cache.reset()

    def connected(self):
        """ check if REPL is connected to the RPC server """
        return self._channel is not None

    def update_dbs(self):
        """ update local db info list from grpc """
        self.all_db = [[x.database_id, x.tag_name, x.forked_from]
                        for x in self._stub.ShowDB(slog_pb2.ShowDBRequest())]
        return self.all_db

    def ping(self):
        req = slog_pb2.PingRequest()
        self._stub.Ping(req)

    # def _csv_request_generator()

    @lru_cache(maxsize=None)
    def upload_csv(self, csv_dir, db_id, writer=Writer()):
        '''
        upload csv to current EDB to some database using tsv_bin tool
        this will upload.
        '''
        # upload to ftp first
        ftp_conn = FTP()
        ftp_conn.connect(self.server_addr, self.ftp_port)
        ftp_conn.login(FTP_DEFAULT_USER, FTP_DEFAULT_PWD)
        def csv_request_generator(csv_hash_map):
            ''' a generator to create gRPC stream from a list of facts file '''
            for csv_fname, file_md5 in csv_hash_map.items():
                rel_name = rel_name_from_file(csv_fname)
                req = slog_pb2.PutCSVFactsRequest()
                req.using_database = db_id
                req.relation_name = rel_name
                req.buckets = 6
                req.bodies.extend([file_md5])
                yield req
        csv_file_paths = []
        if not os.path.exists(csv_dir):
            writer.write("facts location does not exist!")
            return
        if os.path.isdir(csv_dir):
            for fname in os.listdir(csv_dir):
                if os.path.getsize(f'{csv_dir}/{fname}') == 0:
                    continue
                if not fname.endswith('.facts') and not fname.endswith('.csv'):
                    continue
                csv_file_paths.append(f'{csv_dir}/{fname}')
        elif csv_dir.strip().endswith('.facts') or csv_dir.strip().endswith('.csv'):
            base = os.path.basename(csv_dir)
            rel_name = base[:base.rfind('.')]
            if rel_name not in {r[0] for r in self.relations}:
                writer.write(f"current database don't have relation {rel_name}" \
                             " please make sure the fact file has name `<rel_name>.facts`")
            csv_file_paths.append(csv_dir)

        if csv_file_paths == []:
            writer.write("no valid facts file found! NOTICE: all csv facts"
                         " file must has extension `.facts`")
            return
        file_hashes_map = {}
        # upload file first
        for csv_fname in csv_file_paths:
            rel_name = rel_name_from_file(csv_fname)
            file_md5 = digest_file(csv_fname)
            with open(csv_fname, 'rb') as csv_f:
                ftp_conn.storbinary(f'STOR {file_md5}', csv_f)
            file_hashes_map[csv_fname] = file_md5
        response = self._stub.PutCSVFacts(csv_request_generator(file_hashes_map))
        if response.success:
            self.switchto_db(response.new_database)
            writer.ok("✅ ")
            writer.write(f"All relation uploaded. now in database {self.cur_db}")
        else:
            writer.fail("💥")
            writer.write(f" {response.error_msg} fail to update!")
        ftp_conn.close()

    @lru_cache(maxsize=None)
    def compile_slog(self, filename, writer=Writer()):
        '''
        compile a slog file, and set current DB as the resultant DB.
        Returns a 2-tuple of new-db-id and program hashes
        '''
        path = os.path.join(os.getcwd(), filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as file_not_found:
            writer.write(f"Error processing preambles:\n{file_not_found}")
            return None

        self._load(elaborator.hashes)
        inited_db = self._compile(elaborator.hashes.keys(), writer)
        if not inited_db:
            return None
        self.switchto_db(inited_db)
        return inited_db, elaborator.hashes.keys()

    @lru_cache(maxsize=None)
    def slog_add_rule(self, slog_str, db_id, writer:Writer=Writer()):
        """ add a single line of slog query to some database, return the generated database """
        hash_func = hashlib.sha256()
        slog_code = slog_str.encode('utf-8')
        hash_func.update(slog_code)
        slog_hash = hash_func.hexdigest()
        self._load({slog_hash: slog_str})
        inited_db = self._compile([slog_hash], writer)
        if not inited_db:
            return
        new_db = self._run(program_hashes=[slog_hash], input_database=db_id, writer=writer)
        return new_db

    def _load(self, hsh_map: dict):
        ''' load a slog file into backend, input is hash map contain file_hash ↦ file_body '''
        # Exchange hashes
        req = slog_pb2.HashesRequest()
        req.session_key = "empty"
        req.hashes.extend(hsh_map.keys())
        response = self._stub.ExchangeHashes(req)
        req = slog_pb2.PutHashesRequest()
        req.session_key = "empty"
        for hsh in response.hashes:
            req.bodies.extend([hsh_map[hsh]])
        self._stub.PutHashes(req)

    def _compile(self, program_hashes, writer=Writer()):
        ''' compile a slog program list (hashes) and return corresponded EDB '''
        req = slog_pb2.CompileHashesRequest()
        req.buckets = 6
        req.using_database = ""
        req.hashes.extend(program_hashes)
        response = self._stub.CompileHashes(req)
        # Wait to resolve the promise in the terminal...
        # Break when promise is resolved
        edb = self.run_until_promised(response.promise_id, PING_INTERVAL, writer)
        if not edb:
            writer.write("Compilation failed!")
        self.update_dbs()
        return edb

    @lru_cache(maxsize=None)
    def run_with_db(self, filename, db_id=None, cores=1, writer=Writer()):
        ''' run a program with input database '''
        self.update_dbs()
        if not db_id:
            db_id = self.cur_db
        path = os.path.join(os.getcwd(), filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as file_not_found:
            writer.write("Error processing preambles:")
            writer.write(file_not_found)
            return
        if self.lookup_db_by_id(db_id):
            db_id = self.lookup_db_by_id(db_id)[0]
            output_db = self._run(elaborator.hashes.keys(), db_id, cores, writer)
        elif self.lookup_db_by_tag(db_id):
            db_id = self.lookup_db_by_tag(db_id)[0]
            output_db = self._run(elaborator.hashes.keys(), db_id, cores, writer)
        else:
            writer.write("database not exists!")
            return
        if output_db:
            self.switchto_db(output_db)
        return output_db

    def _run(self, program_hashes:list, input_database:str, cores=2, writer=Writer()):
        ''' run a program list (hashes) with given EDB hash and return updated idb'''
        req = slog_pb2.RunProgramRequest()
        req.using_database = input_database
        req.cores = cores
        req.hashes.extend(program_hashes)
        # Get a promise for the running response
        # this memory measure is very in precise make sure no other thing is running...
        memory_prev = psutil.virtual_memory().used
        response = self._stub.RunHashes(req)
        writer.write(f"running promise is {response.promise_id}")
        if response.promise_id == MAXSIZE:
            writer.write("running a file never loaded!")
            return None
        out_db = self.run_until_promised(response.promise_id, PING_INTERVAL, writer)
        if not out_db:
            writer.write("Execution failed!")
        memory_after = psutil.virtual_memory().used
        writer.write(f"System memory increase {(memory_after - memory_prev)/(1024*1024)}MB"
                     " during running")
        self.update_dbs()
        return out_db

    def _update_intern_strings(self, db_id):
        """ update cached string.csv data """
        if self.local_db_path:
            with open(os.path.join(self.local_db_path, '$strings.csv'), 'r') as string_file:
                for s_line in string_file:
                    if s_line.strip() == '':
                        continue
                    sid = s_line.split('\t')[0]
                    sv = s_line.split('\t')[1]
                    self.intern_string_dict[int(sid)] = sv.strip()
            return
        req = slog_pb2.StringRequest()
        req.database_id = db_id
        for sres in self._stub.GetStrings(req):
            self.intern_string_dict[sres.id] = f'"{sres.text}"'

    def switchto_db(self, db_id):
        """ switch to a new database """
        self.db_cache.database(db_id) # Load new database, ignore return

        self.update_dbs()
        if self.lookup_db_by_tag(db_id):
            db_id = self.lookup_db_by_tag(db_id)[0]
        elif self.lookup_db_by_id(db_id):
            db_id = self.lookup_db_by_id(db_id)[0]
        else:
            print(f"database {db_id} not exists!")
            return
        self._cur_time += 1
        self.cur_db = db_id
        new_ts = self._cur_time
        self._times[new_ts] = db_id
        self.load_relations(db_id)
        self._update_intern_strings(self.cur_db)
        self.updated_tuples = {}

    def fresh(self):
        """ go back to ⊥ """
        self.cur_db = None
        self.relations = []
        self.intern_string_dict = {}
        self.slog_tuple_parser = None

    def load_relations(self, db_id):
        """ get all relation inside a database """
        if self.local_db_path:
            for table_file in os.listdir(self.local_db_path):
                if table_file.endswith('.table') or table_file.endswith('.table_full'):
                    rel_info = get_relation_info(os.path.join(self.local_db_path, table_file))
                    self.relations.append([rel_info['name'], rel_info['arity'],
                                           rel_info['tag'], rel_info['num_tuples']])
            # print(self.relations)
            return
        req = slog_pb2.DatabaseRequest()
        req.database_id = db_id
        res = self._stub.GetRelations(req)
        self.relations = [[rel.name, rel.arity, rel.tag, rel.num_tuples] for rel in res.relations]

    def dump_relation_names(self, writer: Writer):
        """ print all relation names, arities, tags, and tuple counts """
        total_tuples = 0
        max_printed_relname = 50
        max_printed_tupcol = 20
        name_hdr = "Relation name"
        max_name_length = min(max_printed_relname,max(map(lambda rel: len(rel[0]), self.relations)))
        name_col_length = max(len(name_hdr) + 2, max_name_length + 2)
        tupcnt_hdr = "# Tuples"
        max_tupcnt_len = min(max_printed_tupcol,max(map(lambda rel: len("{:,}".format(rel[3])), self.relations)))
        tupcnt_col_len = max(len(tupcnt_hdr) + 2, max_name_length + 2)
        arity_hdr = "Arity  "
        arity_hdr_len = len(arity_hdr)
        header = f"{name_hdr: <{name_col_length}}{arity_hdr}{tupcnt_hdr: <{tupcnt_col_len}}Tag    Size (MiB)\n"
        writer.write(header)
        screen_out = ""
        for rel in sorted(self.relations, key=lambda rel: rel[3]*rel[1]):
            tup_cnt = "{:,}".format(rel[3])
            screen_out = screen_out + f"{rel[0] : <{name_col_length}}{rel[1] : <{arity_hdr_len}}{tup_cnt : <{tupcnt_col_len}}{rel[2] : <7}"
            screen_out = screen_out + f"\t{round(rel[3]*rel[1]*8/1024,2)}\n"
            total_tuples += rel[3]
        writer.write(screen_out)
        total_tup_str = "{:,}".format(total_tuples)
        writer.write(f"Total tuples: {total_tup_str}")

    def lookup_db_by_id(self, db_id):
        """ check if a db info record is in cache """
        for db_info in self.all_db:
            if db_info[0].startswith(db_id):
                return db_info
        return False

    def lookup_db_by_tag(self, db_tag):
        """ check if a db info record is in cache """
        for db_info in self.all_db:
            if db_info[1] == db_tag:
                return db_info
        return False

    def lookup_rels(self, name):
        """ check if a relation info is in cache """
        data = []
        for rel in self.relations:
            if rel[0] == name:
                data.append(rel)
        return data

    def lookup_rel_by_tag(self, tag):
        """ check if a relation info is in cache """
        for rel in self.relations:
            if rel[2] == tag:
                return rel

    def _recursive_fetch_tuples(self, tag, tuples_map:dict, db_id):
        """ recursively fecth all tuples of a given relation name """
        if tag in tuples_map.keys():
            return tuples_map
        tuples_map[tag] = []
        if self.local_db_path:
            # local mode
            target_file = None
            for table_file in os.listdir(self.local_db_path):
                if not (table_file.endswith('.table') or table_file.endswith('.table_full')):
                    continue 
                if int(table_file.split('.')[0]) == tag:
                    target_file = table_file
                    break
            arity = int(target_file.split('.')[-2])
            with open(os.path.join(self.local_db_path, target_file), 'rb') as bin_file:
                bin_bytes = bin_file.read()
                u64_buf = []
                for i in range(0, len(bin_bytes), 8):
                    raw_u64 = int.from_bytes(bin_bytes[i:i+8], 'little', signed=False)
                    u64_buf.append(raw_u64)
                    if len(u64_buf) == arity + 1:
                        tuples_map[tag].append(u64_buf[-1])
                        for u64 in u64_buf[:-1]:
                            tuples_map[tag].append(u64)
                        u64_buf = []
            return
        req = slog_pb2.RelationRequest()
        req.database_id = db_id
        req.tag = tag
        for response in self._stub.GetTuples(req):
            for u64 in response.data:
                val_tag = u64 >> 46
                if val_tag > 254:
                    # relation
                    if val_tag not in tuples_map.keys():
                        tuples_map = self._recursive_fetch_tuples(val_tag, tuples_map, db_id)
                tuples_map[tag].append(u64)
        return tuples_map

    @lru_cache(maxsize=None)
    def _dump_tuples(self, name, db_id):
        """ real dump function, this can help use using cahce """
        tuples_map = {}
        for rel in self.lookup_rels(name):
            tag = rel[2]
            self._recursive_fetch_tuples(tag, tuples_map, db_id)
        return tuples_map

    def dump_relation_by_name(self, name, writer:Writer=Writer()):
        """ recursively print all tuples of a relation """
        
        # REFACTOR: Check if relation in cur db
        rels = self.lookup_rels(name)
        if len(rels) == 0:
            writer.write("No relation named {} in the current database".format(name))
            return []

        # Ensure relation is loaded
        relation = self.db_cache.database(self.cur_db).relation_by_name(name)
        relation.load()

        # Create a paginator object, which will subsequently render some 
        # number of tuples at a time
        session = PromptSession()
        paginator = TuplePaginator(writer,relation,self.history,session,5)
        paginator.print_all_tuples(relation)
        
        # there is no way to get what is possible nested relation, 
        # so have to parse whole database first
        # tuples_map = {}
        # for rel in self.relations:
        #     self._recursive_fetch_tuples(rel[2], tuples_map, self.cur_db)
        # # tuples_map = self._dump_tuples(name, self.cur_db)
        # tag_map = {r[2] : (r[0], r[1]) for r in self.relations}
        # tuple_parser = TupleParser(tuples_map, self.group_cardinality, self.unroll_depth,
        #                                 tag_map, self.intern_string_dict)
        # slog_tuples = tuple_parser.parse_query_result()
        # self.slog_tuple_parser = tuple_parser
        # pp_strs = tuple_parser.pretty_str_tuples(rels)
        # for pp_str in pp_strs:
        #     writer.write(pp_str)
        # for rel in rels:
        #     r_tuple_size = len(tuples_map[rel[2]]) / (rel[1] + 1)
        #     writer.write(f"Relation name {name}, tag {rel[2]} has {int(r_tuple_size)} tuples")
        # return slog_tuples

    def tag_db(self, db_id, tag_name):
        """ tag a database with some name """
        request = slog_pb2.TagDBRequest()
        request.database_id = db_id
        request.tag_name = tag_name
        self._stub.TagDB(request)

    def run_until_promised(self, promise_id, ping_interval=1, writer=Writer()):
        '''
        run promise query until promised value returned,
        if writer is provided intermidiate message will be printed
        '''
        cmmt = ""
        while True:
            time.sleep(ping_interval)
            promise_response = slog_pb2.PromiseRequest()
            promise_response.promise_id = promise_id
            res = self._stub.QueryPromise(promise_response)
            if (cmmt is None and res.err_or_db != "") or cmmt != res.err_or_db:
                cmmt = res.err_or_db
                writer.write("✔ {}".format(cmmt))
            elif res.status == STATUS_PENDING:
                continue
            elif res.status == STATUS_FAILED:
                writer.fail("💥")
                writer.write(res.err_or_db)
                return False
            elif res.status == STATUS_RESOLVED:
                writer.ok("✅ ")
                return res.err_or_db
            elif res.status == STATUS_NOSUCHPROMISE:
                writer.fail("💥")
                writer.write("promise does not exist!")
                return

    def desugar_query(self, query_name, query: str):
        '''
        turn a query language into a slog code
        `query_name` is the name of query in generated slog code
        query is query string it self, like `?(foo _ 1)`
        '''
        slog_code = f'[({query_name} x) <-- (= x {query[1:]})]'
        return slog_code

    def run_slog_query(self, query, db_id, writer: Writer):
        """ run a query on some database  """
        hash_func = hashlib.sha256()
        query_encoded = query.encode('utf-8')
        hash_func.update(query_encoded)
        query_hash = hash_func.hexdigest()
        query_name = f"query_{query_hash[:10]}"
        slog_code = self.desugar_query(query_name, query)
        print(slog_code)
        query_db = self.slog_add_rule(slog_code, db_id)
        if not query_db:
            writer.fail(f"query {query} on {db_id} fail!")
            return
        self.switchto_db(query_db)
        tuples_map = self._dump_tuples(query_name, self.cur_db)
        tag_map = {r[2] : (r[0], r[1]) for r in self.relations}
        query_res_rel_tags = self.lookup_rels(query_name)
        tuple_parser = TupleParser(tuples_map, self.group_cardinality, self.unroll_depth,
                                        tag_map, self.intern_string_dict)
        slog_tuples = tuple_parser.parse_query_result()
        # id_print_name_map = {}
        self.slog_tuple_parser = tuple_parser
        # for p_id, p_val in tuple_parser.printed_id_map.items():
        #     id_print_name_map[p_val.tuple_id] = p_id
        # actual_ids = []
        # for slog_tuple in slog_tuples:
        #     if slog_tuple.rel_name == query_name:
        #         actual_ids.append(slog_tuple.col[1][1:])
        # actual_tuples = []
        # for _t in tuple_parser.printed_id_map.values():
        #     # query add layer of helper relation, which will increase the
        #     if _t.tuple_id in actual_ids:
        #         actual_tuples.append(_t)
        return tuple_parser, query_res_rel_tags

    def pretty_print_slog_query(self, query, writer:Writer):
        """
        run a query on current database and print result
        TODO: should we add new database here?
        """
        old_db = self.cur_db
        query_res_parser, query_res_rel_tags = self.run_slog_query(query, self.cur_db, Writer())
        if not query_res_parser:
            return
        pp_strs = query_res_parser.pretty_str_tuples(query_res_rel_tags)
        for pp_str in pp_strs:
            writer.write(pp_str)
        # after dump query relation, delete intermediate database, switch back to old db
        if self.cur_db != old_db:
            # query_db = self.cur_db
            self.switchto_db(old_db)
            # req = slog_pb2.DropDBRequest(database_id=query_db)
            # self._stub.DropDB(req)
        return query_res_parser

    def print_cached_tuple(self, printed_id, writer:Writer):
        """
        fetch  a tuple by it's printed name in query history and print it
        """
        if self.slog_tuple_parser is None or \
           printed_id not in self.slog_tuple_parser.printed_id_map:
            writer.fail(f'requested tuple id {printed_id} is not in query history cache!')
            return
        # tag_map = {r[2] : (r[0], r[1]) for r in self.relations}
        # slog_tuple = self.slog_tuple_parser.printed_id_map[printed_id]
        # pp_str = pretty_str_tuples([slog_tuple], self.unroll_depth, self.group_cardinality,
        #                            tag_map, self.tuple_printed_id_map)
        pp_str = self.slog_tuple_parser.pretty_str_printed_id(printed_id)
        writer.write(pp_str)
        return self.slog_tuple_parser.printed_id_map[printed_id]
