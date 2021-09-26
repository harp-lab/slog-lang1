'''
slog REPL entrance

Kris Micinski
Yihao Sun
'''

import copy
import grpc
from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from prompt_toolkit.completion import WordCompleter, NestedCompleter, PathCompleter, FuzzyWordCompleter
from prompt_toolkit.completion.base import Completer
from prompt_toolkit.document import Document
from prompt_toolkit.formatted_text import HTML
from pyfiglet import Figlet
import time
import timeit
from yaspin import yaspin

from repl.parser import *

# Generated protobufs stuff
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

# How often to wait between pinging the server
PING_INTERVAL = .1

# Constants (see server.py)
STATUS_PENDING  = 0
STATUS_FAILED   = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = -1

BANNER_LOGO = Figlet(font="slant").renderText("   Slog") # thanks to Daniel found this great lib
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
     def get_completions(self, document, complete_event):
        text = document.text_before_cursor
        # stripped_len = len(document.text_before_cursor) - len(text)
        if "\"" in text:
            remain_document = Document(
                text[1:],
                cursor_position=document.cursor_position - 1
            )
            for c in PathCompleter().get_completions(remain_document, complete_event):
                yield c

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
        p = slog_pb2.PromiseRequest()
        p.promise_id = promise_id
        res = stub.QueryPromise(p)
        if (cmmt == None and res.err_or_db != ""):
            cmmt = res.err_or_db
            if spinner: spinner.write("✔ {}".format(cmmt))
        elif (cmmt != res.err_or_db):
            cmmt = res.err_or_db
            spinner.write("✔ {}".format(cmmt))
        elif (res.status == STATUS_PENDING):
            continue
        elif (res.status == STATUS_FAILED):
            if spinner : spinner.fail("💥")
            print(res.err_or_db)
            return False
        elif (res.status == STATUS_RESOLVED):
            if spinner : spinner.ok("✅ ")
            return res.err_or_db


class Repl:
    def __init__(self):
        print(BANNER_LOGO)
        print(BANNER)
        self._channel = None
        self._parser = CommandParser()
        self.reconnect("localhost")
        self.lasterr = None
        self.relations = []
        self.unroll_depth = 3  
        self._cur_edb = None
        self._cur_idb = None
        self._cur_program_hashes = None
        self.intern_string_dict = {}

    def connected(self):
        return self._channel != None
    
    def reconnect(self,server):
        self._server = server
        self._channel = grpc.insecure_channel('{}:5108'.format(server))
        self._stub = slog_pb2_grpc.CommandServiceStub(self._channel)
        self._cur_time = -1
        # Hash from timestamps to database hashes
        self._times    = {}

    def _update_intern_strings(self):
        if not self._cur_edb:
            print("nothing is loaded, can't fectch interned strings")
            return
        req = slog_pb2.StringRequest()
        req.database_id = self._cur_edb
        for sres in self._stub.GetStrings(req):
            self.intern_string_dict[sres.id] = sres.text
    
    def upload_csv(self, csv_dir):
        ''' upload csv to current EDB using tsv_bin tool '''
        if not self._cur_edb:
            print("No edb selected, please run a file to get EDB.")
            return
        csv_file_paths = []
        if os.path.isdir(csv_dir):
            for fname in os.listdir(csv_dir):
                if not fname.endswith('.facts'):
                    continue
                csv_file_paths.append(f'{csv_dir}/{fname}')
        elif csv_dir.strip().endswith('.facts'):
            csv_file_paths.append(csv_dir)
        if csv_file_paths == []:
            print("no valid facts file found! NOTICE: all csv facts file must has extension `.facts`")
            return
        for csv_fname in csv_file_paths:
            rel_name = get_rel_name_souffle_fact(csv_fname)
            with yaspin(text='uploading csv facts ...') as spinner:
                req = slog_pb2.PutCSVFactsRequest()
                req.using_database = self._cur_edb
                req.relation_name = rel_name.encode('utf-8')
                req.buckets = 16
                with open(csv_fname, 'r') as csv_f:
                    csv_text = csv_f.read().encode('utf-8')
                    req.bodies.extend([csv_text])
                    response = self._stub.PutCSVFacts(req)
                    if not response.success:
                        spinner.fail("💥")
                        print(f" {csv_fname} fail to upload!")
                    else:
                        spinner.ok("✅ ")
                        print(f" {csv_fname} uploaded.")

    def fresh(self):
        '''
        run current program using current EDB and reset current IDB
        NOTE: this will silently switch current database to IDB
        '''
        if (not self._cur_edb) or (not self._cur_program_hashes):
            print("Please run(compile) a slog program before fresh database")
            return
        idb = self._run(self._cur_program_hashes, self._cur_edb)
        self._cur_idb = idb
        self.switchto_db(idb)

    def _run(self, program_hashes, edb):
        ''' run a program list (hashes) with given EDB hash and return updated idb'''
        req = slog_pb2.RunProgramRequest()
        req.using_database = edb
        req.hashes.extend(program_hashes)
        # Get a promise for the running response
        response = self._stub.RunHashes(req)
        with yaspin(text="Running...") as spinner:
            idb = run_until_promised(self._stub, response.promise_id, spinner)
            if not idb:
                print("Execution failed!")
            return idb

    def load_slog_file(self, filename):
        ''' load a slog file, and set current file as that one '''
        path = os.path.join(os.getcwd(),filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as e:
            print("Error processing preambles:")
            print(e)
            return
        self._load(elaborator) 
        self._cur_program_hashes = elaborator.hashes.keys()

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

    def compile_and_run(self,filename):
        ''' compile a slog program and run it, also reset current program and EDB/IDB '''
        path = os.path.join(os.getcwd(),filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as e:
            print("Error processing preambles:")
            print(e)
            return
        self._load(elaborator)
        self._cur_program_hashes = elaborator.hashes.keys()
        # Generate a compile request
        edb = self._compile(elaborator.hashes.keys())
        if not edb:
            return
        self._cur_edb = edb
        idb = self._run(elaborator.hashes.keys(), edb)
        if idb:
            self._cur_idb = idb
            self.switchto_db(idb)

    def switchto_db(self,db_id):
        self._cur_time += 1
        self._cur_db = db_id
        new_ts = self._cur_time
        self._times[new_ts] = db_id
        self.load_relations(db_id)
        self._update_intern_strings()
        self.tuples = {}

    def switchto_edb(self):
        if self._cur_edb is None:
            print("No slog file and database is loaded!")
            return
        self.switchto_db(self._cur_edb)
    
    def switchto_idb(self):
        if self._cur_idb is None:
            print("No ouput database, please run slog file first!")
            return
        self.switchto_db(self._cur_idb)

    def load_relations(self,db_id):
        req = slog_pb2.DatabaseRequest()
        req.database_id = db_id
        res = self._stub.GetRelations(req)
        self.relations = []
        for relation in res.relations:
            self.relations.append([relation.name,relation.arity,relation.tag])

    def lookup_rels(self,name):
        data = []
        for r in self.relations:
            if r[0] == name:
                data.append(r)
        return data

    def lookup_rel_by_tag(self, tag):
        for r in self.relations:
            if r[2] == tag:
                return r

    def fetch_tuples(self,name):
        print(name)
        req = slog_pb2.RelationRequest()
        req.database_id = self._cur_db
        arity   = self.lookup_rels(name)[0][1]
        req.tag = self.lookup_rels(name)[0][2]
        n = 0
        x = 0
        tuples = []
        buf = [-1 for i in range(0, arity+1)]
        for response in self._stub.GetTuples(req):
            if (response.num_tuples == 0): continue
            for u64 in response.data:
                if (x == 0):
                    # index col
                    rel_tag = u64 >> 46
                    bucket_hash = (u64 & BUCKET_MASK) >> 28
                    tuple_id = u64 & TUPLE_ID_MASK
                    buf[0] = (rel_tag, bucket_hash, tuple_id)
                    x += 1
                    continue
                val_tag = u64 >> 46
                if val_tag == INT_TAG:
                    attr_val = u64 & VAL_MASK
                elif val_tag == STRING_TAG:
                    attr_val = self.intern_string_dict[u64 & VAL_MASK]
                else:
                    # relation
                    rel_name = self.lookup_rel_by_tag(val_tag)
                    attr_val = f'rel_{rel_name}_{u64 & VAL_MASK}'
                buf[x] = attr_val
                x += 1
                if (x == arity + 1):
                    tuples.append(copy.copy(buf))
                    x = 0
                    n += 1
            assert (n == response.num_tuples)
        self.tuples[name] = tuples
        return tuples

    def recursive_dump_tuples(self,rel):
        for tuple in self.fetch_tuples(rel[0]):
            print("\t".join(map(lambda x: str(x), tuple)))

    def pretty_dump_relation(self,name):
        if (len(self.lookup_rels(name)) == 0):
            print("No relation named {} in the current database".format(name))
            return
        elif (len(self.lookup_rels(name)) > 1):
            print("More than one arity for {}, not currently supporting printing for multi-arity relations".format(name))
            return
        self.recursive_dump_tuples(self.lookup_rels(name)[0])

    def get_front(self):
        if (not self.connected()):
            return "Disconnected"
        elif (self._cur_time == -1):
            return "⊥"
        else:
            return "t{}".format(self._cur_time)

    def loop(self):
        while True:
            try:
                front = self.get_front()
                relation_names = map(lambda x: x[0], self.relations)
                # completer = WordCompleter(relation_names)
                completer_map = {cmd: None for cmd in CMD}
                completer_map['dump'] = FuzzyWordCompleter(list(relation_names))
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
        print('Goodbye.')
        exit(0)

    def calc_ping(self):
        req = slog_pb2.PingRequest()
        start_time = timeit.default_timer()
        res = self._stub.Ping(req)
        end_time = timeit.default_timer()
        elapsed = end_time - start_time
        return elapsed * 1000

    def bottom_toolbar(self):
        ping = self.calc_ping()
        if self.connected():
            return HTML('<style color="lightgreen">[host: <b>{}</b> ping: {:.2f} ms]  [?? jobs in queue]</style>'.format(self._server,self.calc_ping()))
        else:
            return HTML('Disconnected. Use `connect <host>`')

repl = Repl()

try:
    while True:
        repl.loop()
except KeyboardInterrupt:
    repl.exit()
