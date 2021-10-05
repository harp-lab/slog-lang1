'''
slog REPL entrance

Kris Micinski
Yihao Sun
'''

import os
import sys
import time

import timeit
import grpc
from prompt_toolkit import PromptSession
from prompt_toolkit.completion.base import merge_completers
from prompt_toolkit.history import FileHistory
from prompt_toolkit.completion import NestedCompleter, FuzzyWordCompleter
from prompt_toolkit.formatted_text import HTML
from pyfiglet import Figlet
from six import MAXSIZE
from yaspin import yaspin

from slog.common import (STATUS_PENDING, STATUS_FAILED, STATUS_RESOLVED, STATUS_NOSUCHPROMISE,
                         rel_name_from_file, run_until_promised, make_stub)
from slog.common.elaborator import Elaborator
from slog.common.verbs import upload_csvs, run_hashes
import slog.protobufs.slog_pb2 as slog_pb2
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc
from slog.repl.completer import SequencialCompleter, StringPathCompeleter, PrefixWordCompleter
from slog.repl.commands import exec_command, CMD

# How often to wait between pinging the server
PING_INTERVAL = .1

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


class Repl:
    """ Slog REPL """

    def __init__(self):
        self._channel = None
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
        self.all_db = []
        print(BANNER_LOGO)
        print(BANNER)

    def showdbs(self):
        """ print all databases """
        self._fecth_dbs()
        print('tag\tid\tforked from')
        for db_info in self.all_db:
            db_str = f'{db_info[1]}\t{db_info[0][:10]}\t{db_info[2][:10]}'
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

        self._channel, self._stub = make_stub('{}:5108'.format(server))
        self._cur_time = -1
        # Hash from timestamps to database hashes
        self._times = {}

    def upload_csv(self, csv_dir):
        with yaspin(text='uploading csv facts ...') as spinner:
            relset = {r[0] for r in self.relations}
            self._cur_db = upload_csvs(self._stub, self._cur_db, csv_dir, spinner, relset)

    def _run(self, program_hashes, input_database, cores=2):
        ''' run a program list (hashes) with given EDB hash and return updated idb'''
        with yaspin(text="Running...") as spinner:
            return run_hashes(self._stub, program_hashes, input_database, cores, spinner)

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
        self.switchto_db(inited_db)

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

    def run_with_db(self, filename, db_id=None):
        ''' run a program with input database '''
        self._fecth_dbs()
        if not db_id:
            db_id = self._cur_db
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
            self.switchto_db(output_db)

    def switchto_db(self, db_id):
        """ switch to a database """
        self._cur_time += 1
        self._cur_db = db_id
        new_ts = self._cur_time
        self._times[new_ts] = db_id
        self.relations = load_relations(db_id)
        self.intern_string_dict.update(get_intern_strings(self._stub, self._cur_db))

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

    def lookup_rel_by_tag(self, tag):
        """ check if a relation info is in cache """
        for rel in self.relations:
            if rel[2] == tag:
                return rel
        return None

    def pretty_dump_relation(self, name, out_file=None):
        """ recursive print all tuples of a relation """
        dump_relation(cur_db, name, out_file)

    def tag_db(self, db_id, tag_name):
        """ tag a database with some name """
        request = slog_pb2.TagDBRequest()
        request.database_id = db_id
        request.tag_name = tag_name
        self._stub.TagDB(request)

    def get_front(self):
        """ get prompt prefix mark """
        if not self._cur_db:
            return "⊥"
        if not self.connected():
            return "Disconnected"
        else:
            return self._cur_db[:5]

    def loop(self):
        """  REPL main entrance """
        while True:
            try:
                front = self.get_front()
                relation_names = map(lambda x: x[0], self.relations)
                # completer = WordCompleter(relation_names)
                self._fecth_dbs()
                completer_map = {cmd: None for cmd in CMD}
                completer_map['dump'] = FuzzyWordCompleter(list(relation_names))
                possible_db_hash = [db[0][:6] for db in self.all_db]
                possible_db_tag = []
                for db_info in self.all_db:
                    if db_info[1].strip() != "":
                        possible_db_tag.append(db_info[1])
                completer_map['run'] = merge_completers([
                    StringPathCompeleter(),
                    FuzzyWordCompleter(possible_db_hash+possible_db_tag)])
                completer_map['tag'] = SequencialCompleter([
                    FuzzyWordCompleter(possible_db_hash),
                    PrefixWordCompleter('"', possible_db_tag)])
                completer_map['load'] = StringPathCompeleter()
                completer_map['compile'] = StringPathCompeleter()
                completer_map['switch'] = FuzzyWordCompleter(possible_db_hash + possible_db_tag)
                completer = NestedCompleter(completer_map)
                text = prompt_session.prompt(
                    'σλoγ [{}] » '.format(front),
                    bottom_toolbar=self.bottom_toolbar(),
                    complete_while_typing=True,
                    completer=completer)
                if text.strip() == '':
                    continue
                exec_command(self, text)
            except EOFError:
                self.exit()
            except AssertionError:
                return

    def exit(self):
        """ exit REPL """
        print('Goodbye.')
        sys.exit(0)

    def invalid_alert(self, message):
        """ print alert for command exectution """
        print(f"Invalid command: {message}")

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
            return HTML('<style color="lightgreen">'
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
