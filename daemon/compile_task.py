## Compiles Slog code and prepares it for execution
import time

class CompileTask():
    def __init__(self,conn,log):
        self._db = conn
        self._log = log
        
    def log(self,msg):
        print("[ CompileTask {} ] {}".format(time.time(),msg))

    def loop(self):
        self.log("Starting compile task.")
        while True:
            c = self._db.cursor()
            c.execute('SELECT * FROM compile_jobs where STATUS=0')
            rows = c.fetchall()
            for row in rows:
                print(row)
