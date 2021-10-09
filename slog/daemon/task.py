"""
Some slog front/backend tasks

Kris Micinski
Yihao Sun
"""

from subprocess import PIPE
import datetime
import glob
import os
import shutil
import subprocess
import time

import sexpdata
from sexpdata import Symbol

from slog.daemon.db import MetaDatabase
from slog.daemon.const import DB_PATH, COMPILESVC_LOG, SLOG_COMPILER_PROCESS, SOURCES_PATH, \
                         DATABASE_PATH, SLOG_COMPILER_ROOT, BINS_PATH, CMAKE_FILE, RUNSVC_LOG
from slog.daemon.util import generate_db_hash, split_hashes, rel_name_covert, checkpoint_ord


class Task:
    """ Task base class """
    def __init__(self):
        self._db = MetaDatabase(DB_PATH)
        self._name = ""
        self._logfile = None

    def log(self, msg):
        """ task logging """
        curtime = datetime.datetime.now().strftime("(%H:%M:%S %d/%m/%Y)")
        out = "[ {} {} ] {}".format(self._name, curtime, msg)
        print(out)
        self._logfile.write(out + "\n")
        return curtime

    def wait(self):
        """ avoid too fast? """
        time.sleep(.1)

    def set_promise_comment(self, promise, comment):
        """ set promise comment """
        self._db.update_promise_comment(promise, comment)


class CompilerTaskException(Exception):
    """ compiler task exception class """
    # def __init__(self, txt):
    #     super().__init__(txt)


class CompileTask(Task):
    """ Compiling Slog->C++ Task """

    def __init__(self):
        super().__init__()
        self._logfile = COMPILESVC_LOG
        self._name = "CompileTask"
        self.log("Starting compiler subprocess...")
        self._proc = subprocess.Popen(
            ["racket", SLOG_COMPILER_PROCESS], stdin=PIPE, stdout=PIPE)
        line = self._proc.stdout.readline()
        response = sexpdata.loads(line.decode('utf-8'))
        if response == [Symbol('ready')]:
            self.log("Compiler initialized: ready to compile\n")
        else:
            self.log("error: compiler not initialized. Instead of (ready) got back {}\n".format(
                line.decode('utf-8')))
            raise CompilerTaskException("compiler not initialized")

    def compile_to_mpi(self, in_db, out_db, hashes, buckets, promise_id):
        """ compile a program wil given input database and output dir """
        files = " ".join(map(lambda hsh: ("\"" + os.path.join(SOURCES_PATH, hsh) + "\""),
                             hashes))
        self.log(f"Beginning compilation to C++ for hashes {','.join(hashes)},"
                 " generating program for db hash {in_db}")
        # C++ file
        cpp_file = os.path.join(SOURCES_PATH, in_db + "-compiled.cpp")
        indata_directory = os.path.join(DATABASE_PATH, in_db)
        outdata_directory = os.path.join(DATABASE_PATH, out_db)
        # Create output data directory
        os.makedirs(indata_directory, exist_ok=True)
        os.makedirs(outdata_directory, exist_ok=True)
        # Assemble sexpr to send to the running compiler process
        sexpr = "(compile-hashes \"{}\" (files {}) {} \"{}\" \"{}\" \"{}\")".format(
            SLOG_COMPILER_ROOT, files, buckets, cpp_file,
            indata_directory, outdata_directory)
        print(sexpr)
        # Run the compilation command
        self._proc.stdin.write((sexpr + "\n").encode('utf-8'))
        self._proc.stdin.flush()
        line = self._proc.stdout.readline().decode('utf-8')
        try:
            output = sexpdata.loads(line)
        except AssertionError:
            self.log("Slog->C++ compilation failed!")
            self._db.fail_compiled_job(promise_id, line)
            return
        # If compilation failed...
        if output[0] == Symbol('failure'):
            # Log failure
            self.log("Slog->C++ compilation failed!")
            self._db.fail_compiled_job(promise_id, output[1])
        elif output[0] == Symbol('success'):
            # Add the promise for the database association
            self.set_promise_comment(
                promise_id, "Slog -> C++ compilation successful. Now compiling C++...")
            self.log("Slog->C++ compilation successful. Compiling to MPI")
            # go over created input dir, update matdabase
            for fname in os.listdir(indata_directory):
                if fname.endswith('.table'):
                    self._db.create_relation_by_datapath(
                        in_db, os.path.join(indata_directory,fname))
            # persist input database info
            self._db.create_database_info(in_db, "init", "", "")
            return self.compile_cpp(in_db, cpp_file, promise_id)

    def compile_cpp(self, hsh, cppfile, promise_id):
        """  compiled generated C++ file """
        # Create a directory for the build
        build_dir = os.path.join(BINS_PATH, hsh)
        try:
            os.mkdir(build_dir)
        except FileExistsError:
            # File already exists. Ignore for now and overwrite
            self.log(f"Warning: in compiling C++, build directory {build_dir} already exists."
                     " Overwriting")
        # Copy the C++ source to the build directory
        shutil.copy2(cppfile, build_dir)
        # Copy the CMakeList.txt to the build directory
        shutil.copy2(CMAKE_FILE, build_dir)
        print("running cmake now.")
        # Now run cmake
        try:
            cmake = ["cmake", "-Bbuild", "."]
            result = subprocess.run(
                cmake, cwd=build_dir, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, check=True)
        except:
            err = "Error: cmake failed for {}".format(build_dir)
            self._db.fail_compiled_job(promise_id, err)
            self.log(err)
            return
        self.log("cmake completed successfully")
        try:
            make = ["make"]
            result = subprocess.run(
                make, cwd=f"{build_dir}/build", stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                check=True)
        except:
            err = "Error: make failed for {}:\n{}".format(
                build_dir, result.stderr)
            self._db.fail_compiled_job(promise_id, err)
            self.log(err)
            return
        self._db.resolve_compiled_job(promise_id)
        self.set_promise_comment(
            promise_id, "C++ -> binary compilation successful. Returning this initial database.")
        self.log(
            "Compilation -> MPI successful, now finishing promise for subsequent run.")
        return

    def loop(self):
        """ keep fectc pending job """
        while True:
            self.wait()
            rows = self._db.get_all_pending_compile_job()
            for row in rows:
                promise = row[0]
                hashes = row[2]
                in_database_id = row[3]
                out_database_id = row[4]
                buckets = int(row[5])
                self.compile_to_mpi(in_database_id, out_database_id,
                                    split_hashes(hashes), buckets, promise)

class RunTask(Task):
    """ running slog program task """

    def __init__(self,):
        super().__init__()
        self._logfile = RUNSVC_LOG
        self._name = "RunTask"
        self.log("MPI runner initialized.")
        self.relations = []

    def reset_lines(self, database_id):
        """ upldate cached relation in task """
        rows = self._db.get_all_relations_in_db(database_id)
        self.relations = []
        for row in rows:
            self.relations.append([row[0], row[1], row[2]])

    def finalize_run(self, in_db, out_db):
        """
        Check the output DB for consistency and index final outputs
        Return true/false if succeed/fail
        if no final ouptful, check latest check point (by file update time)
        """
        dirs = glob.glob(os.path.join(DATABASE_PATH, out_db, "checkpoint-*"))
        if len(dirs) < 1:
            self.log("Could not find any output at {}".format(
                glob.glob(os.path.join(DATABASE_PATH, out_db, "checkpoint-*"))))
            self.log("try latest checkpoint ...")
            return False
        if os.path.exists(os.path.join(DATABASE_PATH, out_db, "checkpoint-final")):
            checkpoint_dir = os.path.join(DATABASE_PATH, out_db, "checkpoint-final")
        else:
            checkpoint_dir = sorted(dirs, key=checkpoint_ord, reverse=True)[0]
        self.log("Indexing directory {}".format(checkpoint_dir))
        # copy strings.csv
        # find the corresponed input database
        string_file_in = os.path.join(DATABASE_PATH, in_db, '$strings.csv')
        string_file_out = os.path.join(DATABASE_PATH, out_db, '$strings.csv')
        shutil.copy2(string_file_in, string_file_out)
        # get all relations
        relations = self._db.get_all_relations_in_db(in_db)
        # copy all file out of checkpoint
        for relation in relations:
            rel_file_name = "rel__{}__{}__{}_full".format(
                rel_name_covert(relation[0]), relation[1],
                "__".join(map(str, range(1,relation[1]+1))))
            checkpoint_data_file = os.path.join(checkpoint_dir, rel_file_name)
            new_data_file = os.path.join(
                DATABASE_PATH, out_db,
                f'{relation[2]}.{relation[0]}.{relation[1]}.table')
            if not os.path.exists(checkpoint_data_file):
                # touch file
                with open(new_data_file, 'w+') as _:
                    pass
                self.log(f"Output files {checkpoint_data_file} do not exists as expected,"
                          " maybe the output relation has no tuple?")
                # rule maybe empty
                self._db.update_relation_data_info(new_data_file, 0, out_db,
                                                   relation[0], relation[1])
                continue
            # copy file out of checkpoint
            shutil.copy2(checkpoint_data_file, new_data_file)
            # read file size to compute row
            rows = int(os.path.getsize(new_data_file) / ((relation[1] + 1) * 8))
            self._db.update_relation_data_info(checkpoint_data_file, rows, out_db,
                                               relation[0], relation[1])
            self.log(f"Found {rows} rows for relation {relation[0]}.")
        out_db_path = os.path.join(DATABASE_PATH, out_db)
        for fname in os.listdir(out_db_path):
            if fname.endswith('.table'):
                self._db.create_relation_by_datapath(
                    out_db, os.path.join(out_db_path, fname))
        return True

    def run_mpi(self, promise, hsh, in_db, out_db, cores):
        """ run compiled c++ program """
        # compute digest of combined hashe to get build hash
        hashes = split_hashes(hsh)
        build_dir = os.path.join(BINS_PATH, generate_db_hash(hashes))
        in_db_dir = os.path.join(DATABASE_PATH, in_db)
        out_db_dir = os.path.join(DATABASE_PATH, out_db)
        self.log("Starting mpirun for hash {}".format(hsh))
        current_time = int(time.time())
        stdoutfile = "stdout-{}".format(current_time)
        stderrfile = "stderr-{}".format(current_time)
        stdoutpath = os.path.join(in_db_dir, stdoutfile)
        stderrpath = os.path.join(in_db_dir, stderrfile)
        self.set_promise_comment(promise, f"Now beginning MPI run. stdout->{stdoutfile}"
                                          f" stderr->{stderrfile}")
        failed_comment = "MPI process failed."
        # Hack for MPI on OSX
        env = os.environ.copy()
        env["TMPDIR"] = "/tmp"
        failed = False
        if cores < 0:
            cores = 1
        _proc = subprocess.Popen(["mpirun", "-n", str(cores), "./target", in_db_dir, out_db_dir],
                                 stdin=PIPE, stdout=PIPE, stderr=open(stderrpath, 'w'),
                                 cwd=f"{build_dir}/build", env=env)
        with open(stdoutpath, 'w') as stdout_f:
            self.reset_lines(out_db)
            # Process each line of the MPI tasks's output
            while True:
                line = _proc.stdout.readline()
                # EOF
                if not line:
                    break
                stdout_f.write(str(line, 'utf-8'))
        # Done executing mpirun
        _proc.terminate()
        try:
            _proc.wait(timeout=0.2)
        except subprocess.TimeoutExpired:
            failed = True
            failed_comment = 'subprocess did not terminate in time'
            self.log(failed_comment)
        # Check
        failed = failed or _proc.returncode != 0
        self._db.reslove_mpi_job(promise)
        # Index output db if we haven't failed yet
        if ((not failed) and (not self.finalize_run(in_db, out_db))):
            failed = True
            failed_comment = 'Could not finalize run for database from hash {} and DB ID {}'.format(
                hsh, out_db)
        # Update the promise
        if failed:
            self._db.fail_promise(promise, failed_comment)
        else:
            self._db.reslove_promise(promise)
            # persist output database info
            # get input database
            self._db.create_database_info(out_db, "", "", in_db)


    def loop(self):
        """ find all pending running, run forever """
        while True:
            self.wait()
            rows = self._db.get_all_pending_mpi_job()
            for row in rows:
                promise = row[1]
                hsh = row[3]
                in_db = row[4]
                out_db = self._db.get_db_by_promise(promise)
                cores = row[8]
                self.run_mpi(promise, hsh, in_db, out_db, cores)
                # self._db.fail_mpi_job(promise, str(err))
                # self.log("Exception during MPI job {}: {}".format(idx, err))
