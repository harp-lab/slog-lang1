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
                         rel_name_from_file, make_stub)
from slog.common.elaborator import Elaborator
from slog.common.client import SlogClient, ConsoleWriter, FileWriter
import slog.protobufs.slog_pb2 as slog_pb2
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc
from slog.repl.completer import SequencialCompleter, StringPathCompeleter, PrefixWordCompleter
from slog.repl.commands import exec_command, CMD

# thanks to Daniel found this great lib
BANNER_LOGO = Figlet(font="slant").renderText("   Slog")
BANNER = '''
    A Parallel Datalog Engine!
    Type `help` to see help information.

'''

class Repl:
    """ Slog REPL """

    def __init__(self, server=None):
        # TODO: init the SlogClient
        self.client = SlogClient(server)
        self.prompt_session = PromptSession(history=FileHistory("./.slog-history"))
        print(BANNER_LOGO)
        print(BANNER)

    def showdbs(self):
        """ print all databases """
        dbs = self.client.update_dbs()
        headers = [["tag", "id", "parent"]]
        for db_info in (headers + dbs):
            print(f'{db_info[1]:<6} {db_info[0][:10]:<10} {db_info[2][:6]:<6}')

    def connect(self, server):
        self.client.reconnect(server)

    def dump_relations(self, name, out_file=None):
        if out_file:
            with open(out_file, 'w') as f:
                self.client.pretty_dump_relation(name, FileWriter(f))
        else:
            self.client.pretty_dump_relation(name, ConsoleWriter())

    def upload_csv(self, location):
        with yaspin(text='uploading csv facts ...') as spinner:
            self.client.upload_csv(location, writer=spinner)

    def load_slog_file(self, location):
        with yaspin(text="Compiling...") as spinner:
            self.client.load_slog_file(location, writer=spinner)

    def run_with_db(self, location, dbid=None, cores=2):
        with yaspin(text="Running...") as spinner:
            self.client.run_with_db(location, dbid, cores, spinner)

    def tag_db(self, old, new):
        self.client.tag_db(old, new)

    def switch_db(self, to):
        self.client.switchto_db(to)

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
            start_time = timeit.default_timer()
            self.client.ping()
            end_time = timeit.default_timer()
            elapsed = end_time - start_time
        except grpc.RpcError:
            print("can't connect to slog daemon server!")
            sys.exit(1)
        return elapsed * 1000

    def get_front(self):
        """ get prompt prefix mark """
        if not self.client.cur_db:
            return "⊥"
        if not self.client.connected():
            return "Disconnected"
        else:
            return self.client.cur_db[:5]

    def bottom_toolbar(self):
        """ prompt toolkit bottom bar setting """
        if self.client.connected():
            return HTML('<style color="lightgreen">'
                        '[host: <b>{}</b> ping: {:.2f} ms]  [?? jobs in queue]'
                        '</style>'.format(self.client.server, self.calc_ping()))
        else:
            return HTML('Disconnected. Use `connect <host>`')

    def loop(self):
        """  REPL main entrance """
        while True:
            try:
                front = self.get_front()
                relation_names = map(lambda x: x[0], self.client.relations)
                # completer = WordCompleter(relation_names)
                self.client.update_dbs()
                completer_map = {cmd: None for cmd in CMD}
                completer_map['dump'] = FuzzyWordCompleter(list(relation_names))
                possible_db_hash = [db[0][:6] for db in self.client.all_db]
                possible_db_tag = []
                for db_info in self.client.all_db:
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
                text = self.prompt_session.prompt(
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


if __name__ == "__main__":
    # Take server as an optional argument to the repl.
    if len(sys.argv) > 1:
        repl = Repl(sys.argv[1])
    else:
        repl = Repl()
    try:
        while True:
            repl.loop()
    except KeyboardInterrupt:
        repl.exit()
