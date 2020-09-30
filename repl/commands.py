## Command objects execute REPL commands

class Command():
    def __init__(self):
        pass
    
    def execute(self,repl):
        pass

class LoadCommand(Command):
    def __init__(self,filename):
        self.filename = filename
    
    def execute(self,repl):
        repl.load(self.filename)

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
