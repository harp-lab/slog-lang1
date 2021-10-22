"""
These are common 'verbs' for things to do
"""

from functools import lru_cache
from ftplib import FTP
import hashlib
import os
import sys
import time

import grpc
from six import MAXSIZE

from slog.common import rel_name_from_file, make_stub, PING_INTERVAL
from slog.common.elaborator import Elaborator
from slog.common.tuple import parse_query_result, pretty_str_tuples
from slog.daemon.const import FTP_DEFAULT_PWD, FTP_DEFAULT_USER, STATUS_PENDING, STATUS_FAILED, STATUS_RESOLVED, STATUS_NOSUCHPROMISE
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

class SlogClient:
    """
    Client to a slog server.
    """

    def __init__(self, server="localhost", rpc_port=5108, ftp_port=2121):
        self._channel = None
        self._stub = None
        self.server_addr = server
        self.ftp_port = ftp_port
        try:
            self.connect(f"{server}:{rpc_port}")
        except grpc.RpcError:
            print("Can't connect to slog daemon server")
            sys.exit(1)
        self.lasterr = None
        self.relations = []
        self.unroll_depth = 5
        self.group_cardinality = 5
        self.cur_db = ''
        self._cur_program_hashes = None
        self.intern_string_dict = {}
        self.updated_tuples = {}
        self.all_db = []
        # map of tuple with it's print name
        self.tuple_printed_id_map = {}

    def connect(self, server):
        """ Reconnect to the rpc server """
        self.server = server
        self._channel, self._stub = make_stub(server)
        self._cur_time = -1
        # Hash from timestamps to database hashes
        self._times = {}

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
                req.buckets = 16
                req.bodies.extend([file_md5])
                yield req
        csv_file_paths = []
        if not os.path.exists(csv_dir):
            writer.write("facts location does not exist!")
            return
        if os.path.isdir(csv_dir):
            for fname in os.listdir(csv_dir):
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
        req.buckets = 16
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
    def run_with_db(self, filename, db_id=None, cores=2, writer=Writer()):
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
        response = self._stub.RunHashes(req)
        writer.write(f"running promise is {response.promise_id}")
        if response.promise_id == MAXSIZE:
            writer.write("running a file never loaded!")
            return None
        out_db = self.run_until_promised(response.promise_id, PING_INTERVAL, writer)
        if not out_db:
            writer.write("Execution failed!")
        self.update_dbs()
        return out_db

    def _update_intern_strings(self, db_id):
        """ update cached string.csv data """
        req = slog_pb2.StringRequest()
        req.database_id = db_id
        for sres in self._stub.GetStrings(req):
            self.intern_string_dict[sres.id] = f'"{sres.text}"'

    def switchto_db(self, db_id):
        """ switch to a database """
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
        self.cur_db = ""
        self.relations = []
        self.intern_string_dict = {}
        self.tuple_printed_id_map = {}

    def load_relations(self, db_id):
        """ get all relation inside a database """
        req = slog_pb2.DatabaseRequest()
        req.database_id = db_id
        res = self._stub.GetRelations(req)
        self.relations = [[rel.name, rel.arity, rel.tag] for rel in res.relations]

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

    def _recusive_fetch_tuples(self, tag, tuples_map:dict, db_id):
        """ recursively fecth all tuples of a given relation name """
        if tag in tuples_map.keys():
            return tuples_map
        tuples_map[tag] = []
        req = slog_pb2.RelationRequest()
        req.database_id = db_id
        req.tag = tag
        for response in self._stub.GetTuples(req):
            for u64 in response.data:
                val_tag = u64 >> 46
                if val_tag > 254:
                    # relation
                    if val_tag not in tuples_map.keys():
                        tuples_map = self._recusive_fetch_tuples(val_tag, tuples_map, db_id)
                tuples_map[tag].append(u64)
        return tuples_map

    @lru_cache(maxsize=None)
    def _dump_tuples(self, name, db_id):
        """ real dump function, this can help use using cahce """
        tuples_map = {}
        for rel in self.lookup_rels(name):
            tag = rel[2]
            self._recusive_fetch_tuples(tag, tuples_map, db_id)
        return tuples_map

    def dump_relation_by_name(self, name, writer:Writer=Writer()):
        """ recursive print all tuples of a relation """
        rels = self.lookup_rels(name)
        if len(rels) == 0:
            writer.write("No relation named {} in the current database".format(name))
            return []
        tuples_map = self._dump_tuples(name, self.cur_db)          
        tag_map = {r[2] : (r[0], r[1]) for r in self.relations}
        for rel in rels:
            r_tuple_size = len(tuples_map[rel[2]]) / (rel[1] + 1)
            writer.write(f"Relation name {name}, tag {rel[2]} has {int(r_tuple_size)} tuples")
        slog_tuples = parse_query_result(tuples_map, tag_map, self.intern_string_dict,
                                         self.tuple_printed_id_map)
        pp_strs = pretty_str_tuples(slog_tuples, self.unroll_depth, self.group_cardinality,
                                    tag_map, self.tuple_printed_id_map)
        for pp_str in pp_strs:
            writer.write(pp_str)
        return slog_tuples

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
        slog_tuples = parse_query_result(tuples_map, tag_map, self.intern_string_dict,
                                         self.tuple_printed_id_map)
        id_print_name_map = {}
        for p_id, p_val in self.tuple_printed_id_map.items():
            id_print_name_map[p_val.tuple_id] = p_id
        actual_ids = []
        for slog_tuple in slog_tuples:
            if slog_tuple.rel_name == query_name:
                actual_ids.append(slog_tuple.col[1][1:])
        actual_tuples = []
        for _t in self.tuple_printed_id_map.values():
            # query add layer of helper relation, which will increase the
            if _t.tuple_id in actual_ids:
                actual_tuples.append(_t)
        return actual_tuples

    def pretty_print_slog_query(self, query, writer:Writer):
        """
        run a query on current database and print result
        TODO: should we add new database here?
        """
        old_db = self.cur_db
        query_res = self.run_slog_query(query, self.cur_db, Writer())
        writer.write(f"Total {len(query_res)} tuple in results!")
        if not query_res:
            return
        tag_map = {r[2] : (r[0], r[1]) for r in self.relations}
        pp_strs = pretty_str_tuples(query_res, self.unroll_depth, self.group_cardinality,
                                    tag_map, self.tuple_printed_id_map)
        for pp_str in pp_strs:
            writer.write(pp_str)
        # after dump query relation, delete intermediate database, switch back to old db
        if self.cur_db != old_db:
            # query_db = self.cur_db
            self.switchto_db(old_db)
            # req = slog_pb2.DropDBRequest(database_id=query_db)
            # self._stub.DropDB(req)
        return query_res

    def print_cached_tuple(self, printed_id, writer:Writer):
        """
        fetch  a tuple by it's printed name in query history and print it
        """
        if self.tuple_printed_id_map is None or \
           printed_id not in self.tuple_printed_id_map:
            writer.fail(f'requested tuple id {printed_id} is not in query history cache!')
            return
        tag_map = {r[2] : (r[0], r[1]) for r in self.relations}
        slog_tuple = self.tuple_printed_id_map[printed_id]
        pp_str = pretty_str_tuples([slog_tuple], self.unroll_depth, self.group_cardinality,
                                   tag_map, self.tuple_printed_id_map)
        writer.write(pp_str[0])
        return self.tuple_printed_id_map[printed_id]
        