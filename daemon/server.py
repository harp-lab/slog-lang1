# Slog Backend Daemon
from daemon.manifest import *

from concurrent import futures
from sexpdata import Symbol
from subprocess import PIPE
import array
import datetime
import glob
import grpc
import hashlib
import math
import numpy
import os
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc
import sexpdata
import shutil
import sqlite3
import subprocess
import sys
import tempfile
import threading
import time

# Network
PORT = 5106

# Database
DB_PATH = os.path.join(os.path.dirname(__file__),"../metadatabase/database.sqlite3")

# Static databse
DATA_PATH = os.path.join(os.path.dirname(__file__),"../data")
DATABASE_PATH = os.path.join(os.path.dirname(__file__),"../data/databases")
BINS_PATH = os.path.join(os.path.dirname(__file__),"../data/binaries")
CMAKE_FILE = os.path.join(os.path.dirname(__file__),"../data/binaries/CMakeLists.txt")
SOURCES_PATH = os.path.join(os.path.dirname(__file__),"../data/sources")
SLOG_COMPILER_PROCESS = os.path.join(os.path.dirname(__file__),"../compiler/slog-process.rkt")
SLOG_COMPILER_ROOT = os.path.join(os.path.dirname(__file__),"../compiler")

# Logs
CMDSVC_LOG = open(os.path.join(DATA_PATH,"cmdsvc.log"),'a')
COMPILESVC_LOG = open(os.path.join(DATA_PATH,"compilesvc.log"),'a')
RUNSVC_LOG = open(os.path.join(DATA_PATH,"runsvc.log"),'a')

# Statuses
STATUS_PENDING  = 0
STATUS_FAILED   = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = -1

# Compilation timeout in seconds
COMPILATION_TIMEOUT = 20

# Maximum number of bytes allowed per chunk
MAX_CHUNK_DATA = 2097152

# RPC service that responds to commands from the REPL/etc. See protobufs/slog.proto
class CommandService(slog_pb2_grpc.CommandServiceServicer):
    def __init__(self):
        pass

    def log(self,msg):
        out = "[ CommandService {} ] {}".format(datetime.datetime.now().strftime("(%H:%M:%S %d/%m/%Y)"),msg)
        print(out)
        CMDSVC_LOG.write(out + "\n")

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

    def GetTuples(self,request,context):
        self._db = sqlite3.connect(DB_PATH)
        c = self._db.cursor()
        r = c.execute('SELECT name,arity,tag,selection,data_file FROM canonical_relations WHERE database_id = ?',(request.database_id,))
        self.relations.append([row[0],row[1],row[2],list(map(lambda x: int(x), row[3].split(",")))])
        try:
            row = c.fetchone()
            name = row[0]
            arity = row[1]
            tag = row[2]
            selection = map(lambda x: int(x), row[3].split(","))
            data_file = row[4]
            f = open(data_file,'r')
            file_size = os.stat(data_file).st_size
            num_u64s = file_size / 8
            num_tuples = num_u64s / arity
            max_tuples_per_chunk = math.floor(MAX_CHUNK_DATA / (8 * arity))
            num_tuples_left = num_tuples
            while (num_tuples_left > 0):
                buffer = array.array('Q')
                num_tuples = min(num_tuples_left,num_tuples_per_chunk)
                r = Tuples()
                r.status = STATUS_RESOLVED
                r.num_tuples = num_tuples
                buffer.fromfile(f, arity * num_tuples * 8)
                result = buffer.copy()
                remaining = list(range(arity))
                for itm in selection:
                    seq.remove(itm)
                m = seq + remaining
                # Shuffle tuples according
                for n in range(num_tuples):
                    for i in range(arity):
                        copy[n * a + m[i]] = buffer[n*a + i]
                num_tuples_left -=  num_tuples
                r.extend(copy)
                yield r
        except:
            # XXX
            pass
        
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

# 
# Tasks
#

class Task:
    def __init__(self):
        pass
    
    def log(self,msg):
        curtime = datetime.datetime.now().strftime("(%H:%M:%S %d/%m/%Y)")
        out = "[ {} {} ] {}".format(self._name,curtime,msg)
        print(out)
        self._logfile.write(out + "\n")
        return curtime

    def wait(self):
        time.sleep(.1)

    def set_promise_comment(self,promise,comment):
        c = self._db.cursor()
        c.execute('UPDATE promises SET comment = ? WHERE id = ?',(comment,promise))
        self._db.commit()

# 
# Compiling Slog->C++
# 
class CompilerTaskException(Exception):
    def __init__(self,txt):
        super().__init__(txt)

class CompileTask(Task):
    def __init__(self):
        self._logfile = COMPILESVC_LOG
        self._name = "CompileTask"
        self.log("Starting compiler subprocess...")
        self._proc = subprocess.Popen(["racket", SLOG_COMPILER_PROCESS], stdin=PIPE, stdout=PIPE)
        line = self._proc.stdout.readline()
        response = sexpdata.loads(line.decode('utf-8'))
        self.log("here")
        if (response == [Symbol('ready')]):
            self.log("Compiler initialized: ready to compile\n")
        else:
            self.log("error: compiler not initialized. Instead of (ready) got back {}\n".format(line.decode('utf-8')))
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
                pcs = list(map(str,relation[2]))
                name = relation[0].value()
                arity = relation[1]
                c.execute('INSERT INTO canonical_relations (database_id,name,arity,selection,tag,num_tuples) VALUES (?,?,?,?,?,0)',
                          (db_id,name,relation[1],",".join(pcs),str(relation[6])))
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
            self.wait()
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
                try:
                    self.compile_to_mpi(out_hash,hashes,4,promise)
                except Exception as err:
                    r = self.log("Exception for processing compile job {}: {}".format(id,err))
                    s = "Server threw unexpected exception during compilation. Please report this using reference time {}".format(r)
                    c.execute('UPDATE compile_jobs SET status = ?, error = ? WHERE promise = ?',(STATUS_FAILED,str(err),promise))
                    c.execute('UPDATE promises SET status = ?, comment = ? WHERE id = ?',(STATUS_FAILED,s,promise))
                    self._db.commit()

#
# Run task
#

class RunTask(Task):
    def __init__(self,):
        self._logfile = RUNSVC_LOG
        self._name = "RunTask"
        self.log("MPI runner initialized.")

    def reset_lines(self,database_id):
       c = self._db.cursor()
       r = c.execute('SELECT name,arity,tag,selection FROM canonical_relations WHERE database_id = ?',(database_id,))
       rows = c.fetchall()
       self.relations = []
       for row in rows:
           self.relations.append([row[0],row[1],row[2],list(map(lambda x: int(x), row[3].split(",")))])

    def process_line(self,line):
        pass

    # Check the output DB for consistency and index final outputs
    def finalize_run(self,hsh,db_id):
        dirs = glob.glob(os.path.join(DATABASE_PATH,"{}-output".format(hsh),"checkpoint-*"))
        dir = dirs[0]
        print("Found {}".format(dir))
        c = self._db.cursor()
        for relation in self.relations:
            # XXX
            data_file = os.path.join(dir,"rel_{}_{}_{}_full".format(relation[0],relation[1],"_".join(map(lambda x: str(x), relation[3]))))
            size_file = "{}.size".format(data_file)
            if ((not os.path.exists(data_file)) or (not os.path.exists(size_file))):
                self.log("Output files {} or {} do not exists as expected.".format(data_file,size_file))
                return False
            try:
                s = open(size_file,'r')
                lines = s.readlines()
                rows = int(lines[0])
                columns = int(lines[1])
                s.close()
                c.execute('UPDATE canonical_relations SET num_tuples = ?, data_file = ? WHERE database_id = ? AND name = ? AND arity = ?'
                          ,(rows,data_file,db_id,relation[0],relation[1]))
                self._db.commit()
                self.log("Found {} rows for relation {}.".format(rows,relation[0]))
            except:
                self.log("Size file {} was corrupted.".format(size_file))
        return True

    def run_mpi(self,promise,hsh,db_id):
        build_dir = os.path.join(BINS_PATH,hsh)
        self.log("Starting mpirun for hash {}".format(hsh))
        t = time.time()
        stdoutfile = "stdout-{}".format(t)
        stderrfile = "stderr-{}".format(t)
        stdoutpath = os.path.join(build_dir,stdoutfile)
        stderrpath = os.path.join(build_dir,stderrfile)
        self.set_promise_comment(promise,"Now beginning MPI run. stdout->{} stderr->{}".format(stdoutfile,stderrfile))

        # Hack for MPI on OSX
        env = os.environ.copy()
        env["TMPDIR"] = "/tmp"

        failed = False

        self._proc = subprocess.Popen(["mpirun","-n","2","target"],stdin=PIPE, stdout=PIPE, stderr=open(stderrpath,'w'), cwd=build_dir,env=env)
        o = open(stdoutpath,'w')
        self.reset_lines(db_id)

        # Process each line of the MPI tasks's output
        while True:
            line = self._proc.stdout.readline()
            # EOF
            if not line:
                break
            o.write(str(line))
            self.process_line(line)

        # Done executing mpirun
        self._proc.terminate()

        try:
            self._proc.wait(timeout=0.2)
        except subprocess.TimeoutExpired:
            failed = True
            self.log('subprocess did not terminate in time')

        # Check 
        failed = failed or self._proc.returncode != 0
        c = self._db.cursor()
        c.execute('UPDATE mpi_jobs SET status = ? WHERE promise = ?',(STATUS_RESOLVED,promise))

        # Index output db
        failed = failed or (not self.finalize_run(hsh,db_id))

        # Success!
        c.execute('UPDATE promises SET status = ? where id = ?',(STATUS_RESOLVED,promise))
        self._db.commit()
        
    def loop(self):
        self._db = sqlite3.connect(DB_PATH)
        while True:
            self.wait()
            c = self._db.cursor()
            c.execute('SELECT * FROM mpi_jobs where STATUS = ?',(STATUS_PENDING,))
            rows = c.fetchall()
            for row in rows:
                id = row[0]
                promise = row[1]
                status = row[2]
                hsh = row[3]
                creation_time = row[4]
                try:
                    c.execute('SELECT database_id FROM promises_for_databases WHERE promise_id = ?', (promise,))
                    db_id = c.fetchone()[0]
                    self.run_mpi(promise,hsh,db_id)
                except Exception as err:
                    r = self.log("Exception during MPI job {}: {}".format(id,err))
                    s = "Exception during MPI execution. Try again or contact administrator for error log."
                    c.execute('UPDATE mpi_jobs SET status = ?, error = ? WHERE promise = ?',(STATUS_FAILED,str(err),promise))
                    c.execute('UPDATE promises SET status = ?, comment = ? WHERE id = ?',(STATUS_FAILED,s,promise))
                    self._db.commit()


def start_compile_task():
    CompileTask().loop()

def start_mpirun_task():
    RunTask().loop()

# Start the server
server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
slog_pb2_grpc.add_CommandServiceServicer_to_server(CommandService(),server)
print('Slog server starting. Listening on port {}'.format(PORT))
server.add_insecure_port('[::]:{}'.format(PORT))
server.start()

# Start the compile task
compile_task = threading.Thread(target=start_compile_task, daemon=True)
compile_task.start()

# Start the MPI run task
mpirun_task = threading.Thread(target=start_mpirun_task, daemon=True)
mpirun_task.start()

try:
    while True:
        time.sleep(86400)
except KeyboardInterrupt:
    server.stop(0)
    print('Server is exiting.')
