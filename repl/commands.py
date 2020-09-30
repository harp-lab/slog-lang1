## Command objects execute REPL commands
import os, os.path

class Command():
    def __init__(self):
        pass
    
    def execute(self,repl):
        pass

class LoadCommand(Command):
    def __init__(self,filename):
        self.filename = filename
    
    def execute(self,repl):
        path = os.path.join(os.getcwd(),self.filename)
        repl.load(path)

class DefCommand(Command):
    def __init__(self,filename):
        self.filename = filename
    
    def execute(self,repl):
        pass

class ConnectCommand(Command):
    def __init__(self,server):
        self.server = server

    def execute(self,repl):
        repl.reconnect(self.server)
