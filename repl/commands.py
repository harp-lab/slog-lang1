## Command objects execute REPL commands
import os, os.path
from repl.elaborator import *

class Command():
    def __init__(self):
        pass
    
    def execute(self,repl):
        pass

class RunCommand(Command):
    def __init__(self,filename):
        self.filename = filename
    
    def execute(self,repl):
        repl.run(self.filename)

class ConnectCommand(Command):
    def __init__(self,server):
        self.server = server

    def execute(self,repl):
        repl.reconnect(self.server)
