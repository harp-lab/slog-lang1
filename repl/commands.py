'''
Command objects execute REPL commands

Kris Micinski
Yihao Sun
'''

import abc


HELP = '''
    Command:
    help                        Print help
    showdb                      show all committed databases
    compile "<file_path>"       load and compile a slog source file, this will reset database to ⊥
    run "<file_path>" (<db>)    load a slog source file into background, will create a database 
                                with file name, and then compile and run it, if db is not provide
                                will run with current db
    dump <ID>                   dump all data in a relation into stdout           
    connect "<server>"          connect to a slog server
    load "<csv_file/folder>"    upload a csv file/folder into input database, file must ends with 
                                `.fact`, name of target relation will be same as file name
    tag <db> "<tag>"            give a database hash a taged name
'''


class Command(abc.ABC):
    ''' abstract class for command  '''

    @abc.abstractmethod
    def execute(self, repl):
        ''' for repl to run '''


# class RunCommand(Command):
#     def __init__(self, filename):
#         self.filename = filename

#     def execute(self, repl):
#         repl.compile_and_run(self.filename)


class IdCommand(Command):
    ''' dump a relation '''
    def __init__(self, rel_id):
        self.rel_id = rel_id

    def execute(self, repl):
        repl.pretty_dump_relation(self.rel_id)


class ConnectCommand(Command):
    ''' connect to grpc server '''
    def __init__(self, server):
        self.server = server

    def execute(self, repl):
        repl.reconnect(self.server)


class HelpCommand(Command):
    ''' print help '''
    def execute(self, _repl):
        print(HELP)


class NotImplCommand(Command):
    ''' mock command for unimplemented comment '''
    def __init__(self, cmd):
        self.cmd = cmd

    def execute(self, _repl):
        print(f"Command {self.cmd} not implemented yet!")


class LoadCommand(Command):
    ''' upload a csv/csv_folder to input facts database '''

    def __init__(self, csv_dir):
        self.csv_dir = csv_dir

    def execute(self, repl):
        repl.upload_csv(self.csv_dir)


class CompileCommand(Command):
    ''' switch current active file '''

    def __init__(self, filename):
        self.filename = filename

    def execute(self, repl):
        repl.load_slog_file(self.filename)


class ShowDbCommand(Command):
    ''' print all persisted database '''

    def execute(self, repl):
        return repl.showdbs()


class RunWithDbCommand(Command):
    '''' run a program with given input database '''

    def __init__(self, program, db_id=None):
        self.program = program
        print(program)
        self.db_id = db_id

    def execute(self, repl):
        repl.run_with_db(self.program, self.db_id)

class TagCommand(Command):
    "tag a database hash with some name"
 
    def __init__(self, db_id, tag_name):
        self.db_id = db_id
        self.tag_name = tag_name

    def execute(self, repl):
        repl.tag_db(self.db_id, self.tag_name)
