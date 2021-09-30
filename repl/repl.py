'''
slog REPL entrance

Kris Micinski
Yihao Sun
'''

import copy
import os
import sys
import time

import timeit
import grpc
from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from prompt_toolkit.completion import NestedCompleter, PathCompleter, FuzzyWordCompleter
from prompt_toolkit.completion.base import Completer
from prompt_toolkit.document import Document
from prompt_toolkit.formatted_text import HTML
from pyfiglet import Figlet
from six import MAXSIZE
from yaspin import yaspin

from repl.parser import CommandParser, CMD
from repl.elaborator import Elaborator

# Generated protobufs stuff
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

# How often to wait between pinging the server
PING_INTERVAL = .1

# Constants (see server.py)
STATUS_PENDING = 0
STATUS_FAILED = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = 10

# thanks to Daniel found this great lib
BANNER_LOGO = Figlet(font="slant").renderText("   Slog")
BANNER = '''
    A Parallel Datalog Engine!
    Type `help` to see help information.

'''

TAG_MASK = 0xFFFFC00000000000
BUCKET_MASK = 0x00003FFFF0000000
TUPLE_ID_MASK = 0xFFFFFFFFF0000000
VAL_MASK = ~ TAG_MASK
INT_TAG = 0
STRING_TAG = 2
SYMBOL_TAG = 3

# prompt session
prompt_session = PromptSession(history=FileHistory("./.slog-history"))


class StringPathCompeleter(Completer):
    """ completer for '<file path>' """

    def get_completions(self, document, complete_event):
        text = document.text_before_cursor
        # stripped_len = len(document.text_before_cursor) - len(text)
        if "\"" in text:
            remain_document = Document(
                text[1:],
                cursor_position=document.cursor_position - 1
            )
            yield from PathCompleter().get_completions(remain_document, complete_event)


def get_arity_souffle_facts(souffle_fpath):
    ''' get arity of a souffle facts file'''
    with open(souffle_fpath, 'r') as souffle_f:
        fst_line = souffle_f.readline()
    if fst_line.strip() == '':
        return -1
    else:
        return len(fst_line.strip().split('\t'))


def get_rel_name_souffle_fact(souffle_fpath):
    ''' get relation name from a souffle facts file '''
    return souffle_fpath.split('/')[-1][:-6]


def run_until_promised(stub, promise_id, spinner=None):
    '''
    run promise query until promised value returned, if spinner is provide
    intermidiate message will be printed
    '''
    cmmt = ""
    while True:
        time.sleep(PING_INTERVAL)
        promise_response = slog_pb2.PromiseRequest()
        promise_response.promise_id = promise_id
        res = stub.QueryPromise(promise_response)
        if cmmt is None and res.err_or_db != "":
            cmmt = res.err_or_db
            if spinner:
                spinner.write("✔ {}".format(cmmt))
        elif cmmt != res.err_or_db:
            cmmt = res.err_or_db
            spinner.write("✔ {}".format(cmmt))
        elif res.status == STATUS_PENDING:
            continue
        elif res.status == STATUS_FAILED:
            if spinner:
                spinner.fail("💥")
            print(res.err_or_db)
            return False
        elif res.status == STATUS_RESOLVED:
            if spinner:
                spinner.ok("✅ ")
            return res.err_or_db
        elif res.status == STATUS_NOSUCHPROMISE:
            spinner.fail("💥")
            print(f"promise id {promise_id} not exists !")
            return


class Repl:
    """ Slog REPL """

    def __init__(self):
        self._channel = None
        self._parser = CommandParser()
        try:
            self.reconnect("localhost")
        except grpc.RpcError:
            print("Can't connect to slog daemon server")
            sys.exit(1)
        self.lasterr = None
        self.relations = []
        self.unroll_depth = 3
        self._cur_db = ''
        self._cur_program_hashes = None
        self.intern_string_dict = {}
        self.updated_tuples = {}
        self.all_db = []
        print(BANNER_LOGO)
        print(BANNER)

    def showdbs(self):
        """ print all databases """
        self._fecth_dbs()
        print('tag\tid\tforked from')
        for db_info in self.all_db:
            db_str = f'{db_info[1]}\t{db_info[0]}\t{db_info[2]}'
            print(db_str)

    def _fecth_dbs(self):
        """ update local db info list from grpc """
        self.all_db = []
        for db_info in self._stub.ShowDB(slog_pb2.ShowDBRequest()):
            self.all_db.append([db_info.database_id, db_info.tag_name,
                                db_info.forked_from])

    def connected(self):
        """ check if REPL has connect to RPC server """
        return self._channel is not None

    def reconnect(self, server):
        """ reconnect to rpc server """
        self._server = server
        self._channel = grpc.insecure_channel('{}:5108'.format(server))
        self._stub = slog_pb2_grpc.CommandServiceStub(self._channel)
        self._cur_time = -1
        # Hash from timestamps to database hashes
        self._times = {}

    def _update_intern_strings(self):
        """ update cached string.csv data """
        req = slog_pb2.StringRequest()
        req.database_id = self._cur_db
        for sres in self._stub.GetStrings(req):
            self.intern_string_dict[sres.id] = sres.text

    def upload_csv(self, csv_dir):
        ''' upload csv to current EDB using tsv_bin tool '''
        def csv_request_generator(csv_file_paths: list):
            ''' a generator to create gRPC stream from a list of facts file '''
            for csv_fname in csv_file_paths:
                rel_name = get_rel_name_souffle_fact(csv_fname)
                req = slog_pb2.PutCSVFactsRequest()
                req.using_database = self._cur_db
                req.relation_name = rel_name
                req.buckets = 16
                with open(csv_fname, 'r') as csv_f:
                    csv_text = csv_f.read()
                    req.bodies.extend([csv_text])
                yield req
        csv_file_paths = []
        if os.path.isdir(csv_dir):
            for fname in os.listdir(csv_dir):
                if not fname.endswith('.facts'):
                    continue
                csv_file_paths.append(f'{csv_dir}/{fname}')
        elif csv_dir.strip().endswith('.facts'):
            csv_file_paths.append(csv_dir)
        if csv_file_paths == []:
            print("no valid facts file found! NOTICE: all csv facts"
                  " file must has extension `.facts`")
            return
        with yaspin(text='uploading csv facts ...') as spinner:
            response = self._stub.PutCSVFacts(
                csv_request_generator(csv_file_paths))
            if not response.success:
                spinner.fail("💥")
                print(f" {response.error_msg} fail to update!")
            else:
                spinner.ok("✅ ")
                self._cur_db = response.new_database
                print(f"All relation uploaded. now in database {self._cur_db}")

    def _run(self, program_hashes, edb):
        ''' run a program list (hashes) with given EDB hash and return updated idb'''
        req = slog_pb2.RunProgramRequest()
        req.using_database = edb
        req.hashes.extend(program_hashes)
        # Get a promise for the running response
        response = self._stub.RunHashes(req)
        if response.promise_id == MAXSIZE:
            print("running a file never loaded!")
            return
        with yaspin(text="Running...") as spinner:
            idb = run_until_promised(self._stub, response.promise_id, spinner)
            if not idb:
                print("Execution failed!")
            return idb

    def load_slog_file(self, filename):
        ''' load a slog file, and set current file as that one '''
        path = os.path.join(os.getcwd(), filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as file_not_found:
            print("Error processing preambles:")
            print(file_not_found)
            return
        self._load(elaborator)
        inited_db = self._compile(elaborator.hashes.keys())
        self._cur_db = inited_db

    def _load(self, eloborated_file: Elaborator):
        ''' load a eloborated slog file into backend '''
        # Exchange hashes
        req = slog_pb2.HashesRequest()
        req.session_key = "empty"
        req.hashes.extend(eloborated_file.hashes.keys())
        response = self._stub.ExchangeHashes(req)
        req = slog_pb2.PutHashesRequest()
        req.session_key = "empty"
        for hsh in response.hashes:
            req.bodies.extend([eloborated_file.hashes[hsh]])
        self._stub.PutHashes(req)

    def _compile(self, program_hashes):
        ''' compile a slog program list (hashes) and return corresponded EDB '''
        req = slog_pb2.CompileHashesRequest()
        req.buckets = 16
        req.using_database = ""
        req.hashes.extend(program_hashes)
        response = self._stub.CompileHashes(req)
        # Wait to resolve the promise in the terminal...
        with yaspin(text="Compiling...") as spinner:
            # Break when promise is resolved
            edb = run_until_promised(self._stub, response.promise_id, spinner)
            if not edb:
                print("Compilation failed!")
            return edb

    def compile_and_run(self, filename):
        ''' compile a slog program and run it, also reset current program and EDB/IDB '''
        path = os.path.join(os.getcwd(), filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as file_not_found:
            print("Error processing preambles:")
            print(file_not_found)
            return
        self._load(elaborator)
        self._cur_program_hashes = elaborator.hashes.keys()
        # Generate a compile request
        inited_db = self._compile(elaborator.hashes.keys())
        if not inited_db:
            return
        self._cur_db = inited_db
        output_db = self._run(elaborator.hashes.keys(), inited_db)
        if output_db:
            self._cur_db = output_db
            self.switchto_db(output_db)

    def run_with_db(self, filename, db_id):
        ''' run a program with input database '''
        self._fecth_dbs()
        print(self.all_db)
        path = os.path.join(os.getcwd(), filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as file_not_found:
            print("Error processing preambles:")
            print(file_not_found)
            return
        if self.lookup_db_by_id(db_id):
            db_id = self.lookup_db_by_id(db_id)[0]
            output_db = self._run(elaborator.hashes.keys(), db_id)
        elif self.lookup_db_by_tag(db_id):
            db_id = self.lookup_db_by_tag(db_id)[0]
            output_db = self._run(elaborator.hashes.keys(), db_id)
        else:
            print("database not exists!")
            return
        if output_db:
            self._cur_db = output_db
            self.switchto_db(output_db)

    def switchto_db(self, db_id):
        """ switch to a database """
        self._cur_time += 1
        self._cur_db = db_id
        new_ts = self._cur_time
        self._times[new_ts] = db_id
        self.load_relations(db_id)
        self._update_intern_strings()
        self.updated_tuples = {}

    def load_relations(self, db_id):
        """ get all relation inside a database """
        req = slog_pb2.DatabaseRequest()
        req.database_id = db_id
        res = self._stub.GetRelations(req)
        self.relations = []
        for relation in res.relations:
            self.relations.append(
                [relation.name, relation.arity, relation.tag])

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

    def fetch_tuples(self, name):
        """ print all tulple of a relation """
        # print(name)
        req = slog_pb2.RelationRequest()
        req.database_id = self._cur_db
        arity = self.lookup_rels(name)[0][1]
        req.tag = self.lookup_rels(name)[0][2]
        n = 0
        x = 0
        tuples = []
        buf = [-1 for i in range(0, arity+1)]
        for response in self._stub.GetTuples(req):
            if (response.num_tuples == 0):
                continue
            for u64 in response.data:
                if (x == 0):
                    # index col
                    # rel_tag = u64 >> 46
                    # bucket_hash = (u64 & BUCKET_MASK) >> 28
                    tuple_id = u64 & (~TUPLE_ID_MASK)
                    # buf[0] = (rel_tag, bucket_hash, tuple_id)
                    buf[0] = tuple_id
                    x += 1
                    continue
                val_tag = u64 >> 46
                if val_tag == INT_TAG:
                    attr_val = u64 & VAL_MASK
                elif val_tag == STRING_TAG:
                    attr_val = self.intern_string_dict[u64 & VAL_MASK]
                else:
                    # relation
                    rel_name = self.lookup_rel_by_tag(val_tag)[0]
                    # attr_val = f'rel_{rel_name}_{u64 & (~TUPLE_ID_MASK)}'
                    if name != rel_name and rel_name not in self.updated_tuples.keys():
                        self.fetch_tuples(rel_name)
                    attr_val = ['NESTED', rel_name, u64 & (~TUPLE_ID_MASK)]
                buf[x] = attr_val
                x += 1
                if x == arity + 1:
                    # don't print id col
                    # rel name at last
                    tuples.append(copy.copy(buf)+[name])
                    x = 0
                    n += 1
            assert n == response.num_tuples
        self.updated_tuples[name] = tuples
        return tuples

    def recursive_dump_tuples(self, rel):
        """ recursive print all tuples of a relation """
        # reset all tuples to non-updated
        def find_val_by_id(name, row_id):
            for row in self.updated_tuples[name]:
                if row[0] == row_id:
                    return row
        resolved_relname = []

        def _resolve(rname):
            if rname in resolved_relname:
                return
            for i, row in enumerate(self.updated_tuples[rname]):
                for j, col in enumerate(row[:-1]):
                    if not isinstance(col, list):
                        continue
                    if col[0] == 'NESTED':
                        nested_name = col[1]
                        nested_id = col[2]
                        val = find_val_by_id(nested_name, nested_id)
                        if val is None:
                            val = f'"{nested_name} has no fact with id {nested_id} !"'
                        _resolve(nested_name)
                        self.updated_tuples[rname][i][j] = val
            resolved_relname.append(rname)

        def rel_to_str(rel):
            res = []
            for col in rel[:-1]:
                if isinstance(col, type):
                    if col[0] == 'NESTED':
                        res.append(f"({' '.join([str(v) for v in col])})")
                    else:
                        res.append(rel_to_str(col))
                else:
                    res.append(str(col))
            return f"({rel[-1]} {' '.join(res)})"
        self.fetch_tuples(rel[0])
        # print(self.updated_tuples)
        # _resolve(rel[0])
        for fact_row in sorted(self.updated_tuples[rel[0]], key=lambda t: int(t[0])):
            print(f"#{fact_row[0]}:  {rel_to_str(fact_row[1:])}")

    def pretty_dump_relation(self, name):
        """ recursive print all tuples of a relation """
        if len(self.lookup_rels(name)) == 0:
            print("No relation named {} in the current database".format(name))
            return
        elif len(self.lookup_rels(name)) > 1:
            print(f"More than one arity for {name}, not currently"
                  " supporting printing for multi-arity relations")
            return
        self.recursive_dump_tuples(self.lookup_rels(name)[0])

    def get_front(self):
        """ get prompt prefix mark """
        if not self.connected():
            return "Disconnected"
        elif self._cur_db == "":
            return "⊥"
        else:
            return self._cur_db[:5]

    def loop(self):
        """  REPL main entrance """
        while True:
            try:
                front = self.get_front()
                relation_names = map(lambda x: x[0], self.relations)
                # completer = WordCompleter(relation_names)
                completer_map = {cmd: None for cmd in CMD}
                completer_map['dump'] = FuzzyWordCompleter(
                    list(relation_names))
                completer_map['run'] = StringPathCompeleter()
                completer_map['csv'] = StringPathCompeleter()
                completer_map['load'] = StringPathCompeleter()
                completer = NestedCompleter(completer_map)
                text = prompt_session.prompt(
                    'σλoγ [{}] » '.format(front),
                    bottom_toolbar=self.bottom_toolbar(),
                    complete_while_typing=True,
                    completer=completer)
                if text.strip() == '':
                    continue
                cmd = self._parser.parse(text)
                if cmd:
                    cmd.execute(self)
            except EOFError:
                self.exit()
            except AssertionError:
                return

    def exit(self):
        """ exit REPL """
        print('Goodbye.')
        sys.exit(0)

    def calc_ping(self):
        """ calculate ping time to slog rpc server """
        try:
            req = slog_pb2.PingRequest()
            start_time = timeit.default_timer()
            self._stub.Ping(req)
            end_time = timeit.default_timer()
            elapsed = end_time - start_time
        except grpc.RpcError:
            print("can't connect to slog daemon server!")
            sys.exit(1)
        return elapsed * 1000

    def bottom_toolbar(self):
        """ prompt toolkit bottom bar setting """
        if self.connected():
            return HTML(f'<style color="lightgreen">'
                        '[host: <b>{}</b> ping: {:.2f} ms]  [?? jobs in queue]'
                        '</style>'.format(self._server, self.calc_ping()))
        else:
            return HTML('Disconnected. Use `connect <host>`')


if __name__ == "__main__":
    repl = Repl()
    try:
        while True:
            repl.loop()
    except KeyboardInterrupt:
        repl.exit()
