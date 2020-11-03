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
from sexpdata import loads

PORT = 5106
DB_PATH = os.path.join(os.path.dirname(__file__),"../metadatabase/database.sqlite3")
DATA_PATH = os.path.join(os.path.dirname(__file__),"../data")
SOURCES_PATH = os.path.join(os.path.dirname(__file__),"../data/sources")
SLOG_COMPILER_PROCESS = os.path.join(os.path.dirname(__file__),"../compiler/slog-process.rkt")
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
            r = c.execute('SELECT hash FROM hashes where hash=?',(h,))
            if (r.fetchone() == None):
                res.hashes.extend([h])
        return res

    def PutHashes(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        bodies = request.bodies
        ret = slog_pb2.ErrorResponse()
        ret.success = True
        for body in bodies:
            h = hashlib.sha256()
            h.update(body.encode('utf-8'))
            hsh = h.hexdigest()
            fname = os.path.join(SOURCES_PATH, hsh)
            # Check to see if hash exists before adding..
            r = c.execute('SELECT hash FROM hashes where hash=?',(hsh,))
            if (r.fetchone() == None):
                with open(fname, "w") as f:
                    f.write(body)
                    self.log("Writing file for {}".format(hsh))
                    c.execute('INSERT INTO hashes (hash,filename) VALUES (?,?)', (hsh,fname))
                    self._db.commit()
        return ret

    def RunHashes(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        ret = slog_pb2.Promise()
        # Check that all hashes are present
        for hsh in request.hashes:
            r = c.execute('SELECT hash FROM hashes where hash=?',(hsh,))
            if (r.fetchone() == None):
                ret.success = False
                return ret
        hashes = set()
        for hsh in request.hashes:
            print(hsh)
            hashes.add(hsh)
        hashes = list(hashes)
        hashes.sort()
        hashes = (",").join(hashes)
        c.execute('INSERT INTO promises (status,comment,creation_time) VALUES (?,?,time(\'now\'))',
                  (STATUS_PENDING, "Compiling to PRAM IR"))
        promise_id = c.lastrowid
        c.execute('INSERT INTO compile_jobs (promise, status, hashes, creation_time) VALUES (?,?,?,time(\'now\'))',(promise_id,STATUS_PENDING,hashes))
        compile_job_id = c.lastrowid
        self._db.commit()
        print("Made promise {} for new compile job on hashes {}\n".format(promise_id,hashes))
        response = slog_pb2.Promise()
        response.success = True
        response.promise_id = promise_id
        return response

server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))

slog_pb2_grpc.add_CommandServiceServicer_to_server(CommandService(),server)

class MalformedManifest(Exception):
    pass

# Parsing manifest files
class Manifest():
    def __init__(self,filename):
        self.relations = {}
        with open(filename,'r') as f:
            sexpr = loads(f.read())
            if (sexpr[0] != Symbol('manifest')):
                raise MalformedManifest()
            directores = sexpr[1]
            if (directories[0] != Symbol('directories')):
                raise MalformedManifest()
            per_rel = directories[1]
            for rel in per_rel:
                if (rel[0] != Symbol('rel-select-file')):
                    raise MalformedManifest()
                canonical = rel[1]
                name = rel[2]
                arity = rel[3]
                select = rel[4]
                data = rel[5]
                size = rel[6]
                self.relations[[name,arity,select]] = [canonical,data,size]
            string_pool = directories[2][1]
            for entry in string_pool:
                self.strings[entry[0]] = entry[1]

# Compiling Slog->C++
class CompileTask():
    def __init__(self,conn,log):
        self._log = log
        self.log("Loading compiler...")
        self._proc = subprocess.Popen(["racket", SLOG_COMPILER_PROCESS])

    def log(self,msg):
        print("[ CompileTask {} ] {}".format(time.time(),msg))

    def compile_to_cpp(self,out_hash,hashes,job_nodes):
        self.log("Beginning compilation to C++ for {} nodes (dir: {})".format(job_nodes,root_directory))
        output_cpp = os.path.join(SOURCES_PATH,"")
        # Execute the Racket process to perform compilation
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
                hashes = row[3]
                creation_time = row[4]
                h = hashlib.sha256()
                h.update(hashes) # hashes.encode('utf-8')?
                out_hash = h.hexdigest()
                hashes = hashes.split(",")
                self.compile_to_cpp(out_hash,hashes,4)

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
