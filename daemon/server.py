import grpc
from concurrent import futures
import time
import threading
import sqlite3
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc
import os
import sys
import subprocess
import tempfile
import hashlib
from daemon.compile_task import *

PORT = 5106
DB_PATH = os.path.join(os.path.dirname(__file__),"../metadatabase/database.sqlite3")
DATA_PATH = os.path.join(os.path.dirname(__file__),"../data")
SOURCES_PATH = os.path.join(os.path.dirname(__file__),"../data/sources")
SLOG_COMPILER = os.path.join(os.path.dirname(__file__),"../compiler/slog.rkt")
conn = sqlite3.connect(DB_PATH)
log = sys.stderr

STATUS_PENDING  = 0
STATUS_ERROR    = 1
STATUS_RESOLVED = 2

class CommandService(slog_pb2_grpc.CommandServiceServicer):
    def __init__(self):
        self._db = sqlite3.connect(DB_PATH)

    def log(self,msg):
        print("[ CommandService {} ] {}".format(time.time(),msg))

    def gen_data_directory(self):
        return tempfile.mkdtemp(prefix=DATA_PATH+"/")

    def ExchangeHashes(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        l = request.hashes
        res = slog_pb2.Hashes()
        for h in request.hashes:
            print(h)
            r = c.execute('SELECT hash FROM hashes where hash=?',(h,))
            if (r.fetchone() == None):
                res.hashes.extend([h])
        return res

    def PutHashes(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        bodies = request.bodies
        for body in bodies:
            h = hashlib.sha256()
            h.update(body.encode('utf-8'))
            hsh = h.hexdigest()
            fname = os.path.join(SOURCES_PATH, hsh)
            with open(fname, "w") as f:
                f.write(body)
                self.log("Writing file for {}".format(hsh))
                c.execute('INSERT INTO hashes (hash,filename) VALUES (?,?)', (hsh,fname))
                self._db.commit()

    def LoadProgram(self,request,context):
        print("here")
        print(request)
        data_directory = self.gen_data_directory()
        src_directory = os.path.join(data_directory,"src")
        os.mkdir(src_directory,mode=0o700)
        src_file = os.path.join(src_directory,"program.slog")
        print(src_file)
        with open(src_file,"w") as fh:
            fh.write(request.source_program)
        c = self._db.cursor()
        c.execute('INSERT INTO promises (status,comment,creation_time) VALUES (?,?,time(\'now\'))',
                  (STATUS_PENDING, "Compiling to PRAM IR"))
        promise_id = c.lastrowid
        print(promise_id)
        c.execute('INSERT INTO compile_jobs (promise, status, root_directory, creation_time) VALUES (?,?,?,time(\'now\'))',(promise_id,STATUS_PENDING,data_directory))
        compile_job_id = c.lastrowid
        self._db.commit()
        print(compile_job_id)
        print("Wrote {}\n".format(src_file))
        response = slog_pb2.Promise()
        response.success = True
        response.promise_id = promise_id
        return response

server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))

slog_pb2_grpc.add_CommandServiceServicer_to_server(CommandService(),server)

class CompileTask():
    def __init__(self,conn,log):
        self._log = log
        
    def log(self,msg):
        print("[ CompileTask {} ] {}".format(time.time(),msg))

    def compile_to_cpp(self,root_directory,job_nodes):
        self.log("Beginning compilation to C++ for {} nodes (dir: {})".format(job_nodes,root_directory))
        program = os.path.join(root_directory,"src/program.slog")
        # Execute the Racket process to perform compilation
        result = subprocess.run(["racket", SLOG_COMPILER, "-b", str(job_nodes), "-c", "--data", root_directory,program])
        if result.returncode == 0:
            self.log("Slog->C++ compilation successful. Compiling to MPI")
        else:
            self.log("Slog->C++ compilation failed!")

    def loop(self):
        self._db = sqlite3.connect(DB_PATH)
        self.log("Starting compile task.")
        while True:
            c = self._db.cursor()
            c.execute('SELECT * FROM compile_jobs where STATUS=0')
            rows = c.fetchall()
            for row in rows:
                id = row[0]
                promise = row[1]
                status = row[2]
                root_directory = row[3]
                creation_time = row[4]
                self.compile_to_cpp(root_directory,4)

def start_compile_task():
    CompileTask(conn,log).loop()

print('Slog server starting. Listening on port {}'.format(PORT))
server.add_insecure_port('[::]:{}'.format(PORT))

# Start the compile task
compile_task = threading.Thread(target=start_compile_task, daemon=True)
compile_task.start()

# Start the server
server.start()

try:
    while True:
        time.sleep(86400)
except KeyboardInterrupt:
    server.stop(0)
    print('Server is exiting.')
