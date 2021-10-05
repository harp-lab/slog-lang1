'''
slog REPL entrance

Kris Micinski
Yihao Sun
'''

import sys

import timeit
import grpc
from prompt_toolkit import PromptSession
from prompt_toolkit.completion.base import merge_completers
from prompt_toolkit.history import FileHistory
from prompt_toolkit.completion import NestedCompleter, FuzzyWordCompleter
from prompt_toolkit.formatted_text import HTML
from pyfiglet import Figlet
from yaspin import yaspin
from slog.common.client import SlogClient, ConsoleWriter, FileWriter
from slog.repl.completer import SequencialCompleter, StringPathCompeleter, PrefixWordCompleter

# thanks to Daniel found this great lib
BANNER_LOGO = Figlet(font="slant").renderText("   Slog")
BANNER = '''
    A Parallel Datalog Engine!
    Type `help` to see help information.

'''

HELP = '''
    Command:
    NOTE: `(...)` means optionanl argument, `/` means alternative argument, `<...>` is meta name

    help                                Print help

    showdb                              Show all committed databases

    compile "<file>"                    Load and compile a slog source file.

    run "<file>" (<db>) (<core>)        Load a slog source file into background, will create a database
                                        with file name, and then compile and run it, if db is not provide
                                        will run with current db, core is how many core mpirun use.

    dump <hash>/"<tag>" ("<file>")      Dump all data in a relation into stdout, if optional file argument
                                        is provided, result will be printed to file 

    connect "<server>"                  Connect to a slog server

    load "<csv_file/folder>"            Upload a csv file/folder into input database, file must ends with
                                        `.fact`, name of target relation will be same as file name

    tag <hash> "<tag>"                  Give a database hash a taged name

    switch <db>                         Switch to a given DB
'''

CMD = ['help', 'run', 'connect', 'dump', 'showdb',
       'load', 'commit', 'compile', 'tag', 'switch']


def invalid_alert(message):
    """ print alert for command exectution """
    print(f"Invalid command: {message}")

def exec_command(client: SlogClient, raw_input: str):
    """
    A naive valiadator and processer for command
    """
    raw_input = raw_input.strip()
    if raw_input == '':
        # bypass empty command
        return
    # check if it a query
    if raw_input.startswith('?(') and raw_input.endswith(')'):
        return
    # normal command
    cmd = raw_input.split(' ')[0].strip()
    args = [r.strip() for r in raw_input.split(' ')[1:] if r.strip() != '']
    if cmd == 'help':
        print(HELP)
    elif cmd == 'showdb':
        dbs = client.update_dbs()
        headers = [["tag", "id", "parent"]]
        for db_info in headers + dbs:
            print(f'{db_info[1]:<6} {db_info[0][:10]:<10} {db_info[2][:6]:<6}')
    elif cmd == 'connect':
        if len(args) == 1:
            client.connect(args[0])
        else:
            invalid_alert(f'{cmd} expect 1 arg, but get {len(args)}')
    elif cmd == 'dump':
        if len(args) == 1:
            client.pretty_dump_relation(args[0], ConsoleWriter())
        elif len(args) == 2:
            if args[1].startswith('"') and args[1].endswith('"'):
                with open(args[1][1:-1], 'w') as out_f:
                    client.pretty_dump_relation(args[0], FileWriter(out_f))
            else:
                invalid_alert(f'{cmd} expect a string at postion 2 as arg')
        else:
            invalid_alert(f'{cmd} expect 1/2 arg, but get {len(args)}')
    elif cmd == 'load':
        if len(args) == 1:
            if args[0].startswith('"') and args[0].endswith('"'):
                with yaspin(text='uploading csv facts ...') as spinner:
                    client.upload_csv(args[0][1:-1], writer=spinner)
            else:
                invalid_alert(f'{cmd} expect a string at postion 1 as arg')
        else:
            invalid_alert(f'{cmd} expect 1 arg, but get {len(args)}')
    elif cmd == 'compile':
        if len(args) == 1:
            if args[0].startswith('"') and args[0].endswith('"'):
                with yaspin(text="Compiling...") as spinner:
                    client.compile_slog(args[0][1:-1], spinner)
            else:
                invalid_alert(f'{cmd} expect a string at postion 1 as arg')
        else:
            invalid_alert(f'{cmd} expect 1 arg, but get {len(args)}')
    elif cmd == 'run':
        if len(args) > 1 and (not args[0].startswith('"') or not args[0].endswith('"')):
            client.invalid_alert(f'{cmd} expect a string at postion 1 as arg')
            return
        with yaspin(text="Running...") as spinner:
            if len(args) == 2 and len(args[1]) < 5 and args[1].isnumeric():
                client.run_with_db(args[0][1:-1], cores=int(args[1]), writer=spinner)
            elif len(args) == 2:
                client.run_with_db(args[0][1:-1], args[1], writer=spinner)
            elif len(args) == 3 and args[2].isnumeric():
                client.run_with_db(args[0][1:-1], args[1], int(args[2]), writer=spinner)
            else:
                invalid_alert(f'{cmd} has wrong args, please see help')
    elif cmd == 'tag':
        if len(args) == 2:
            if args[1].startswith('"') and args[1].endswith('"'):
                client.tag_db(args[0], args[1][1:-1])
            else:
                invalid_alert(f'{cmd} expect a string at postion 1 as arg')
        else:
            invalid_alert(f'{cmd} expect 2 arg, but get {len(args)}')  
    elif cmd == 'switch':
        if len(args) == 1:
            client.switchto_db(args[0])
        else:
            invalid_alert(f'{cmd} expected 1 argument, got {len(args)}.')
    else:
        invalid_alert(f'{cmd} is not a valid command, type `help to see help`')

class Repl:
    """ Slog REPL """

    def __init__(self, server=None):
        # TODO: init the SlogClient
        self.client = SlogClient(server)
        self.prompt_session = PromptSession(history=FileHistory("./.slog-history"))
        print(BANNER_LOGO)
        print(BANNER)

    def lookup_rel_by_tag(self, tag):
        """ check if a relation info is in cache """
        for rel in self.relations:
            if rel[2] == tag:
                return rel

    def fetch_tuples(self, name):
        """ print all tulple of a relation """
        req = slog_pb2.RelationRequest()
        req.database_id = self._cur_db
        arity = self.lookup_rels(name)[0][1]
        req.tag = self.lookup_rels(name)[0][2]
        row_count = 0
        col_count = 0
        tuples = []
        buf = [-1 for _ in range(0, arity+1)]
        for response in self._stub.GetTuples(req):
            if response.num_tuples == 0:
                continue
            for u64 in response.data:
                if col_count == 0:
                    # index col
                    # rel_tag = u64 >> 46
                    bucket_id = (u64 & BUCKET_MASK) >> 28
                    tuple_id = u64 & (~TUPLE_ID_MASK)
                    buf[0] = (bucket_id, tuple_id, row_count)
                    col_count += 1
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
                    bucket_id = (u64 & BUCKET_MASK) >> 28
                    tuple_id = u64 & (~TUPLE_ID_MASK)
                    attr_val = ['NESTED', rel_name, (bucket_id, tuple_id)]
                buf[col_count] = attr_val
                col_count += 1
                if col_count == arity + 1:
                    # don't print id col
                    # rel name at last
                    tuples.append(copy.copy(buf)+[name])
                    col_count = 0
                    row_count += 1
            assert row_count == response.num_tuples
        self.updated_tuples[name] = tuples
        return tuples

    def recursive_dump_tuples(self, rel, out_path):
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
        if not out_path:
            for fact_row in sorted(self.updated_tuples[rel[0]], key=lambda t: int(t[0][2])):
                print(f"#{fact_row[0][2]}:  {rel_to_str(fact_row[1:])}")
        else:
            with open(out_path, 'w') as out_f:
                for fact_row in sorted(self.updated_tuples[rel[0]], key=lambda t: int(t[0][2])):
                    out_f.write(f"#{fact_row[0][2]}:  {rel_to_str(fact_row[1:])}")

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
                exec_command(self.client, text)
            except EOFError:
                self.exit()
            except AssertionError:
                return


if __name__ == "__main__":
    # Take server as an optional argument to the repl.
    if len(sys.argv) > 1:
        repl = Repl(sys.argv[1])
    else:
        repl = Repl("localhost:5108")
    try:
        while True:
            repl.loop()
    except KeyboardInterrupt:
        repl.exit()
