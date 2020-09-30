## Compiles Slog code and prepares it for execution

class CompileTask():
    def __init__(self,conn,log):
        self._db = conn
        self._log = log
        
    def log(self,msg):
        print("[ CompileTask ] {}".format(msg))

    def loop(self):
        self.log("Starting compile task.")

