'''
Command objects execute REPL commands

Kris Micinski
Yihao Sun
'''

import abc

from repl.elaborator import *

HELP = '''
    Command:
    help                        Print help
    refresh                     rerun program, refresh database
    edb                         switch to EDB(a.k.a. input fact database)
    idb                         switch to IDB(a.k.a. output database)
    showdb                      show all committed databases
    load "<file_path>"          load a slog source file / switch current active file to that
    run "<file_path>"           load a slog source file into background, will create a database 
                                with file name, and then compile and run it
    commit                      commit current database
    dump <ID>                   dump all data in a relation into stdout           
    connect "<server>"          connect to a slog server
    csv "<csv_file/folder>"     upload a csv file/folder into input database, file must ends with 
                                `.fact`, name of target relation will be same as file name
'''

class Command(abc.ABC):
    ''' abstract class for command  '''

    @abc.abstractmethod
    def execute(self,repl):
        ''' for repl to run '''


class RunCommand(Command):
    def __init__(self,filename):
        self.filename = filename
    def execute(self,repl):
        repl.compile_and_run(self.filename)

class IdCommand(Command):
    def __init__(self,id):
        self.id = id
    def execute(self,repl):
        repl.pretty_dump_relation(self.id)

class ConnectCommand(Command):
    def __init__(self,server):
        self.server = server
    def execute(self,repl):
        repl.reconnect(self.server)

class HelpCommand(Command):
    def execute(self, _repl):
       print(HELP)

class NotImplCommand(Command):
    def __init__(self, cmd):
        self.cmd = cmd
    def execute(self, _repl):
       print(f"Command {self.cmd} not implemented yet!")

class CsvCommand(Command):
    ''' upload a csv/csv_folder to input facts database '''
    def __init__(self, csv_dir):
        self.csv_dir = csv_dir
    def execute(self, repl):
        repl.upload_csv(self.csv_dir)

class EdbCommand(Command):
    """ switch to EDB """
    def execute(self, repl):
        repl.switchto_edb()

class IdbCommand(Command):
    """ switch to IDB """
    def execute(self, repl):
        repl.switchto_idb()

class LoadCommand(Command):
    ''' switch current active file '''
    def __init__(self,filename):
        self.filename = filename
    def execute(self,repl):
        repl.load_slog_file(self.filename)

class RefreshCommand(Command):
    ''' rerun program, refresh db '''
    def execute(self,repl):
        repl.fresh()
