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
            r = c.execute('SELECT hash FROM slog_source_files where hash=?',(h,))
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
            if (status == STATUS_RESOLVED):
                r = c.execute('SELECT database_id FROM promises_for_databases WHERE promise_id = ?',(request.promise_id,))
                rows = c.fetchall()
                db_id = rows[0][0]
                res.err_or_db = db_id
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
            r = c.execute('SELECT hash FROM slog_source_files where hash=?',(hsh,))
            if (r.fetchone() == None):
                with open(fname, "w") as f:
                    f.write(body)
                    self.log("Writing file for {}".format(hsh))
                    c.execute('INSERT INTO slog_source_files (hash,filename) VALUES (?,?)', (hsh,fname))
                    self._db.commit()
        return ret

    def RunHashes(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        ret = slog_pb2.Promise()
        # Check that all hashes are present
        for hsh in request.hashes:
            r = c.execute('SELECT hash FROM slog_source_files where hash=?',(hsh,))
            if (r.fetchone() == None):
                ret.promise_id = -1
                return ret
        hashes = set()
        for hsh in request.hashes:
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
        self.log("Made promise {} for new compile job on hashes {}\n".format(promise_id,hashes))
        response = slog_pb2.Promise()
        response.promise_id = promise_id
        return response

    def GetRelations(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        r = c.execute('SELECT name,arity,tag FROM canonical_relations WHERE database_id = ?',(request.database_id,))
        rows = c.fetchall()
        res = slog_pb2.RelationDescriptionsResponse()
        res.success = True
        for row in rows:
            d = slog_pb2.RelationDescription()
            d.name = row[0]
            d.arity = row[1]
            d.tag = row[2]
            res.relations.extend([d])
        return res

server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))

slog_pb2_grpc.add_CommandServiceServicer_to_server(CommandService(),server)

class MalformedManifest(Exception):
    pass

# Parsing manifest files
class Manifest():
    def __init__(self,filename):
        self.relations = []
        with open(filename,'r') as f:
            sexpr = sexpdata.loads(f.read())
            if (sexpr[0] != Symbol('manifest')):
                raise MalformedManifest()
            directories = sexpr[1]
            if (directories[0] != Symbol('directories')):
                raise MalformedManifest()
            per_rel = directories[1]
            for rel in per_rel:
                if (rel[0] != Symbol('rel-select-file')):
                    raise MalformedManifest()
                canonical = (rel[1] == Symbol('canonical'))
                name = rel[2]
                arity = rel[3]
                select = rel[4]
                data = rel[5]
                size_file = rel[6]
                tag  = rel[7]
                self.relations.append([name,arity,select,canonical,data,size_file,tag])
            string_pool = sexpr[2][1]
            for entry in string_pool:
                self.strings[entry[0]] = entry[1]

class CompilerTaskException(Exception):
    def __init__(self,txt):
        super().__init__(txt)

class Task:
    def __init__(self):
        pass
    
    def log(self,msg):
        print("[ {} {} ] {}".format(self._name,time.time(),msg))

    def set_promise_comment(self,promise,comment):
        c = self._db.cursor()
        c.execute('UPDATE promises SET comment = ? WHERE id = ?',(comment,promise))
        self._db.commit()

# Compiling Slog->C++
class CompileTask(Task):
    def __init__(self,conn,log):
        self._name = "CompileTask"
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

        # Run the compilation command
        self._proc.stdin.write((sexpr + "\n").encode('utf-8'))
        self._proc.stdin.flush()
        line = self._proc.stdout.readline().decode('utf-8')
        output = sexpdata.loads(line)

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
            self.set_promise_comment(promise_id,"Slog -> C++ compilation successful. Now compiling C++...")
            self.log("Slog->C++ compilation successful. Compiling to MPI")
            c = self._db.cursor()
            # Add the promise for the database association
            c.execute('INSERT INTO promises_for_databases (promise_id, database_id) VALUES (?,?)',(promise_id,stored_hash))
            manifest_file = os.path.join(indata_directory,"manifest")
            self.load_manifest(stored_hash,manifest_file)
            return self.compile_cpp(stored_hash,outfile,promise_id)

    # Populates the canonical_relations table
    def load_manifest(self,db_id,manifest_file):
        c = self._db.cursor()
        #try:
        #except:
        #self.log("Could not successfully parse manifest {}".format(manifest_file))
        m = Manifest(manifest_file)
        for relation in m.relations:
            # Ignore non-canonical relations, we don't need to record data for those
            is_canonical = relation[3]
            if is_canonical:
                sel = map(str,relation[2])
                c.execute('INSERT INTO canonical_relations (database_id,name,arity,selection,tag) VALUES (?,?,?,?,?)',
                          (db_id,str(relation[0]),relation[1],",".join(sel),str(relation[6])))
        self._db.commit()

    def compile_cpp(self,hsh,cppfile,promise_id):
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
        
        # Now run cmake
        try:
            cmake = ["cmake", "."]
            result = subprocess.run(cmake, cwd=build_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
        except:
            c = self._db.cursor()
            err = "Error: cmake failed for {}".format(build_dir)
            c.execute('UPDATE compile_jobs SET status = ?, error = ? WHERE promise = ?',(STATUS_FAILED,err,promise_id))
            c.execute('UPDATE promises SET status = ?, comment = ? WHERE id = ?',(STATUS_FAILED,err,promise_id))
            self._db.commit()
            self.log(err)
            return

        self.log("cmake completed successfully")
        
        try:
            make = ["make"]
            result = subprocess.run(make, cwd=build_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
        except:
            c = self._db.cursor()
            err = "Error: make failed for {}:\n{}".format(build_dir,result.stderr)
            c.execute('UPDATE compile_jobs SET status = ?, error = ? WHERE promise = ?',(STATUS_FAILED,err,promise_id))
            c.execute('UPDATE promises SET status = ?, comment = ? WHERE id = ?',(STATUS_FAILED,err,promise_id))
            self._db.commit()
            self.log(err)
            return

        c = self._db.cursor()
        c.execute('UPDATE compile_jobs SET status = ? WHERE promise = ?',(STATUS_RESOLVED,promise_id))
        c.execute('INSERT INTO mpi_jobs (promise, status, hash, creation_time) VALUES (?,?,?,time(\'now\'))',(promise_id,STATUS_PENDING,hsh))
        self._db.commit()
        self.set_promise_comment(promise_id,"C++ -> binary compilation successful. Queuing to run")
        self.log("make completed successfully. Queuing binary to run.")
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

# Running tasks via MPI
class RunTask(Task):
    def __init__(self,conn,log):
        self._name = "RunTask"
        self.log("MPI runner initialized.")

    def run_mpi(self,promise,hsh):
        build_dir = os.path.join(BINS_PATH,hsh)

        self.log("Starting mpirun for hash {}".format(hsh))
        self.set_promise_comment(promise,"Now beginning MPI run.")
        # Hack for OSX
        env = os.environ.copy()
        env["TMPDIR"] = "/tmp"
        self._proc = subprocess.Popen(["mpirun","-n","2","target"],stdin=PIPE, stdout=PIPE,cwd=build_dir,env=env)
        while True:
            line = self._proc.stdout.readline()
            # EOF
            if not line:
                break
            print(line)
        # Done executing mpirun
        self._proc.terminate()
        try:
            self._proc.wait(timeout=0.2)
        except subprocess.TimeoutExpired:
            self.log('subprocess did not terminate in time')

        c = self._db.cursor()
        c.execute('UPDATE mpi_jobs SET status = ? WHERE promise = ?',(STATUS_RESOLVED,promise))
        c.execute('UPDATE promises SET status = ? where id = ?',(STATUS_RESOLVED,promise))
        self._db.commit()
        
    def loop(self):
        self._db = sqlite3.connect(DB_PATH)
        while True:
            c = self._db.cursor()
            c.execute('SELECT * FROM mpi_jobs where STATUS = ?',(STATUS_PENDING,))
            rows = c.fetchall()
            for row in rows:
                id = row[0]
                promise = row[1]
                status = row[2]
                hsh = row[3]
                creation_time = row[4]
                self.run_mpi(promise,hsh)
    

def start_compile_task():
    CompileTask(conn,log).loop()

def start_mpirun_task():
    RunTask(conn,log).loop()

print('Slog server starting. Listening on port {}'.format(PORT))
server.add_insecure_port('[::]:{}'.format(PORT))

# Start the compile task
compile_task = threading.Thread(target=start_compile_task, daemon=True)
compile_task.start()

# Start the MPI run task
mpirun_task = threading.Thread(target=start_mpirun_task, daemon=True)
mpirun_task.start()


# Start the server
server.start()

try:
    while True:
        time.sleep(86400)
except KeyboardInterrupt:
    server.stop(0)
    print('Server is exiting.')
