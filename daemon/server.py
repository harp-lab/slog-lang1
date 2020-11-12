# Slog Backend Daemon
import grpc
from concurrent import futures
import time
import threading
import sqlite3
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc
import os
import shutil
import subprocess
from subprocess import PIPE
import sys
import tempfile
import hashlib
from daemon.compile_task import *
import sexpdata
from sexpdata import Symbol

PORT = 5106
DB_PATH = os.path.join(os.path.dirname(__file__),"../metadatabase/database.sqlite3")
DATA_PATH = os.path.join(os.path.dirname(__file__),"../data")
DATABASE_PATH = os.path.join(os.path.dirname(__file__),"../data/databases")
BINS_PATH = os.path.join(os.path.dirname(__file__),"../data/binaries")
CMAKE_FILE = os.path.join(os.path.dirname(__file__),"../data/binaries/CMakeLists.txt")
SOURCES_PATH = os.path.join(os.path.dirname(__file__),"../data/sources")
SLOG_COMPILER_PROCESS = os.path.join(os.path.dirname(__file__),"../compiler/slog-process.rkt")
SLOG_COMPILER_ROOT = os.path.join(os.path.dirname(__file__),"../compiler")
conn = sqlite3.connect(DB_PATH)
log = sys.stderr

STATUS_PENDING  = 0
STATUS_FAILED   = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = -1

# compilation timeout in seconds
COMPILATION_TIMEOUT = 20

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

    def Ping(self,request,context):
        p = slog_pb2.Pong()
        return p

    def QueryPromise(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        r = c.execute('SELECT status,comment FROM promises WHERE id = ?',(request.promise_id,))
        res = slog_pb2.PromiseStatus()
        rows = c.fetchall()
        if (len(rows) == 0):
            res.status = STATUS_NOSUCHPROMISE
            return res
        else:
            status = rows[0][0]
            comment = rows[0][1]
            res.status = status
            res.err_or_db = comment
            return res

    def PutHashes(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        bodies = request.bodies
        ret = slog_pb2.ErrorResponse()
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
                ret.promise_id = -1
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
            sexpr = sexpdata.loads(f.read())
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

class CompilerTaskException(Exception):
    def __init__(self,txt):
        super().__init__(txt)

# Compiling Slog->C++
class CompileTask():
    def __init__(self,conn,log):
        self._log = log
        self.log("Starting compiler subprocess...")
        self._proc = subprocess.Popen(["racket", SLOG_COMPILER_PROCESS],stdin=PIPE, stdout=PIPE)
        line = self._proc.stdout.readline()
        response = sexpdata.loads(line.decode('utf-8'))
        if (response == [Symbol('ready')]):
            self.log("Compiler initialized: ready to compile")
        else:
            self.log("error: compiler not initialized. Instead of (ready) got back {}".format(line.decode('utf-8')))
            raise CompilerTaskException("compiler not initialized")

    def log(self,msg):
        print("[ CompileTask {} ] {}".format(time.time(),msg))


    # compile hashes to an out_hash using job_nodes and refering promise_id
    def compile_to_mpi(self,out_hash,hashes,job_nodes,promise_id):
        hashes.sort()
        output_cpp = os.path.join(SOURCES_PATH,"")
        cathashes = "|".join(hashes).encode('utf-8')
        h = hashlib.sha256()
        h.update(cathashes)
        stored_hash = h.hexdigest()
        files = " ".join(map(lambda hsh: ("\"" + os.path.join(SOURCES_PATH, hsh) + "\""), hashes))

        self.log("Beginning compilation to C++ for hashes {}, generating program w/ hash {}".format(",".join(hashes), stored_hash))
        
        # Number of processes
        nprocesses = 4
        
        # C++ file
        outfile = os.path.join(SOURCES_PATH, stored_hash + "-compiled.cpp")

        # Databases on the filesystem that will be written
        indata_directory = os.path.join(DATABASE_PATH,stored_hash)
        outdata_directory  = os.path.join(DATABASE_PATH,stored_hash + "-output")

        # Create output data directory
        os.makedirs(indata_directory, exist_ok=True)
        os.makedirs(outdata_directory, exist_ok=True)

        # Assemble sexpr to send to the running compiler process
        sexpr = "(compile-hashes \"{}\" (files {}) {} \"{}\" \"{}\" \"{}\")".format(SLOG_COMPILER_ROOT,files,nprocesses,outfile,indata_directory,outdata_directory)

        print(sexpr)
        print(self._proc)

        # Run the compilation command
        self._proc.stdin.write((sexpr + "\n").encode('utf-8'))
        self._proc.stdin.flush()
        line = self._proc.stdout.readline().decode('utf-8')
        output = sexpdata.loads(line)
        print(output)

        # If compilation failed...
        if (output[0] == Symbol('failure')):
            # Log failure 
            self.log("Slog->C++ compilation failed!")
            c = self._db.cursor()
            c.execute('UPDATE compile_jobs SET status = ?, error = ? WHERE promise = ?',(STATUS_FAILED,output[1],promise_id))
            c.execute('UPDATE promises SET status = ?, comment = ? WHERE id = ?',(STATUS_FAILED,output[1],promise_id))
            self._db.commit()
            return
        elif (output[0] == Symbol('success')):
            self.log("Slog->C++ compilation successful. Compiling to MPI")
            c = self._db.cursor()
            c.execute('UPDATE compile_jobs SET status = ? WHERE promise = ?',(STATUS_RESOLVED,promise_id))
            return self.compile_cpp(stored_hash,outfile)

    def compile_cpp(self,hsh,cppfile):
        # Create a directory for the build
        build_dir = os.path.join(BINS_PATH,hsh)
        try:
            os.mkdir(build_dir)
        except FileExistsError:
            # File already exists. Ignore for now and overwrite
            self.log("Warning: in compiling C++, build directory {} already exists. Overwriting".format(build_dir))
        
        # Copy the C++ source to the build directory
        shutil.copy2(cppfile,build_dir)
        
        # Copy the CMakeList.txt to the build directory
        shutil.copy2(CMAKE_FILE,build_dir)
        
        return

    def loop(self):
        self._db = sqlite3.connect(DB_PATH)
        while True:
            c = self._db.cursor()
            c.execute('SELECT * FROM compile_jobs where STATUS = ?',(STATUS_PENDING,))
            rows = c.fetchall()
            for row in rows:
                id = row[0]
                promise = row[1]
                status = row[2]
                hashes = row[3]
                creation_time = row[4]
                h = hashlib.sha256()
                h.update(hashes.encode('utf-8'))
                out_hash = h.hexdigest()
                hashes = hashes.split(",")
                self.compile_to_mpi(out_hash,hashes,4,promise)

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
