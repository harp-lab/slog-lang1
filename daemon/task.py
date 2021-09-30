"""
Some slog front/backend tasks

Kris Micinski
Yihao Sun
"""

from subprocess import PIPE
import datetime
import glob
import os
import re
import shutil
import subprocess
import time

import sexpdata
from sexpdata import Symbol

from daemon.db import MetaDatabase
from daemon.const import DB_PATH, COMPILESVC_LOG, SLOG_COMPILER_PROCESS, SOURCES_PATH, \
                         DATABASE_PATH, SLOG_COMPILER_ROOT, BINS_PATH, CMAKE_FILE, RUNSVC_LOG


def split_hashes(hashes):
    """ split 2 hash values? """
    return hashes.split(",")


def rel_name_covert(rel_name: str):
    """ convert a relation name in code into name in output """
    return rel_name.replace('_', '__').replace('.', '_dot')


def checkpoint_ord(check_dir):
    """ order of checkpoint folder """
    check_stamp = re.findall(r'checkpoint-(\d+)-(\d+)', check_dir)
    if len(check_stamp) == 0:
        return [0, 0]
    else:
        return [int(check_stamp[0][1]), int(check_stamp[0][0])]


class Task:
    """ Task base class """
    def __init__(self):
        self._db = MetaDatabase(DB_PATH)
        self._name = ""
        self._logfile = None

    def log(self, msg):
        curtime = datetime.datetime.now().strftime("(%H:%M:%S %d/%m/%Y)")
        out = "[ {} {} ] {}".format(self._name, curtime, msg)
        print(out)
        self._logfile.write(out + "\n")
        return curtime

    def wait(self):
        """ avoid too fast? """
        time.sleep(.1)

    def set_promise_comment(self, promise, comment):
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
        files = " ".join(
            map(lambda hsh: ("\"" + os.path.join(SOURCES_PATH, hsh) + "\""), hashes))
        self.log(f"Beginning compilation to C++ for hashes {','.join(hashes)},"
                  " generating program for db hash {in_db}")
        # C++ file
        outfile = os.path.join(SOURCES_PATH, out_db + "-compiled.cpp")
        indata_directory = os.path.join(DATABASE_PATH, in_db)
        outdata_directory = os.path.join(DATABASE_PATH, out_db)
        # Create output data directory
        os.makedirs(indata_directory, exist_ok=True)
        os.makedirs(outdata_directory, exist_ok=True)
        # Assemble sexpr to send to the running compiler process
        sexpr = "(compile-hashes \"{}\" (files {}) {} \"{}\" \"{}\" \"{}\")".format(
            SLOG_COMPILER_ROOT, files, buckets, outfile, indata_directory, outdata_directory)
        print(sexpr)
        # Run the compilation command
        self._proc.stdin.write((sexpr + "\n").encode('utf-8'))
        self._proc.stdin.flush()
        line = self._proc.stdout.readline().decode('utf-8')
        output = sexpdata.loads(line)
        # If compilation failed...
        if output[0] == Symbol('failure'):
            # Log failure
            self.log("Slog->C++ compilation failed!")
            self._db.fail_compiled_job(promise_id, output[1])
        elif output[0] == Symbol('success'):
            self.set_promise_comment(
                promise_id, "Slog -> C++ compilation successful. Now compiling C++...")
            self.log("Slog->C++ compilation successful. Compiling to MPI")
            # Add the promise for the database association
            manifest_file = os.path.join(indata_directory, "manifest")
            # Copy the manifest from in to out
            shutil.copy2(manifest_file, os.path.join(
                outdata_directory, "manifest"))
            # In/out db use same manifest, index them now
            self._db.load_manifest(in_db, manifest_file)
            self._db.load_manifest(out_db, os.path.join(outdata_directory, "manifest"))
            # persist input database info
            self._db.create_database_info(in_db, "⊥", "", "")
            return self.compile_cpp(in_db, outfile, promise_id)

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
                cmake, cwd=build_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True)
        except:
            err = "Error: cmake failed for {}".format(build_dir)
            self._db.fail_compiled_job(promise_id, err)
            self.log(err)
            return
        self.log("cmake completed successfully")
        try:
            make = ["make"]
            result = subprocess.run(
                make, cwd=f"{build_dir}/build",stdout=subprocess.PIPE, stderr=subprocess.PIPE,
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
                self.compile_to_mpi(
                    in_database_id, out_database_id, split_hashes(hashes), buckets, promise)

#
# Run task
#


class RunTask(Task):
    """ running slog program task """

    def __init__(self,):
        super().__init__()
        self._logfile = RUNSVC_LOG
        self._name = "RunTask"
        self.log("MPI runner initialized.")

    def reset_lines(self, database_id):
        rows = self._db.get_all_relations_in_db(database_id)
        self.relations = []
        for row in rows:
            self.relations.append([row[0], row[1], row[2], list(
                map(lambda x: int(x), row[3].split(",")))])

    def finalize_run(self, hsh, db_id):
        """
        Check the output DB for consistency and index final outputs
        Return true/false if succeed/fail
        if no final ouptful, check latest check point (by file update time)
        """
        dirs = glob.glob(os.path.join(DATABASE_PATH, db_id, "checkpoint-*"))
        if len(dirs) < 1:
            self.log("Could not find any output at {}".format(
                glob.glob(os.path.join(DATABASE_PATH, db_id, "checkpoint-*"))))
            self.log("try latest checkpoint ...")
            return False
        dir = sorted(dirs, key=checkpoint_ord, reverse=True)[0]
        self.log("Indexing directory {}".format(dir))
        # copy strings.csv
        # find the corresponed input database
        input_db = self._db.get_input_db_reverse(hsh, db_id)
        string_file_in = os.path.join(DATABASE_PATH, input_db, '$strings.csv')
        string_file_out = os.path.join(DATABASE_PATH, db_id, '$strings.csv')
        shutil.copy2(string_file_in, string_file_out)
        for relation in self.relations:
            # if relation[0].startswith('$inter'):
            #     # bypass intermediate relation
            #     continue
            rel_file_name = "rel__{}__{}__{}_full".format(
                rel_name_covert(relation[0]), relation[1],
                "__".join(map(str, relation[3]))
            )
            data_file = os.path.join(dir, rel_file_name)
            size_file = "{}.size".format(data_file)
            new_data_file = os.path.join(DATABASE_PATH, db_id, data_file)
            new_size_file = os.path.join(
                DATABASE_PATH, db_id, f'{data_file}.size')
            print(data_file)
            if ((not os.path.exists(data_file)) or (not os.path.exists(size_file))):
                # touch file
                with open(new_data_file, 'w+') as _:
                    pass
                with open(new_size_file, 'w+') as size_f:
                    size_f.write(f'0\n{relation[1]}\n')
                self.log(
                    f"Output files {data_file} or {size_file} do not exists as expected,"
                     " maybe the output relation has no tuple?")
                # rule maybe empty
                self._db.update_relation_data_info(
                    new_data_file, 0, db_id, relation[0], relation[1])
                continue
            try:
                # copy file out of checkpoint
                shutil.copy2(data_file, new_data_file)
                shutil.copy2(size_file, new_size_file)
                with open(size_file, 'r') as s:
                    lines = s.readlines()
                rows = int(lines[0])
                self._db.update_relation_data_info(
                    data_file, rows, db_id, relation[0], relation[1])
                self.log("Found {} rows for relation {}.".format(
                    rows, relation[0]))
            except:
                self.log("Size file {} was corrupted.".format(size_file))
        return True

    def run_mpi(self, promise, hsh, db_id):
        """ run compiled c++ program """
        build_dir = os.path.join(BINS_PATH, hsh)
        self.log("Starting mpirun for hash {}".format(hsh))
        current_time = int(time.time())
        stdoutfile = "stdout-{}".format(current_time)
        stderrfile = "stderr-{}".format(current_time)
        stdoutpath = os.path.join(build_dir, stdoutfile)
        stderrpath = os.path.join(build_dir, stderrfile)
        self.set_promise_comment(
            promise, "Now beginning MPI run. stdout->{} stderr->{}".format(stdoutfile, stderrfile))
        failed_comment = "MPI process failed."
        # Hack for MPI on OSX
        env = os.environ.copy()
        env["TMPDIR"] = "/tmp"
        failed = False
        _proc = subprocess.Popen(["mpirun", "-n", "2", "target"], stdin=PIPE,
                                      stdout=PIPE, stderr=open(stderrpath, 'w+'),
                                      cwd=f"{build_dir}/build", env=env)
        with open(stdoutpath, 'w') as stdout_f:
            self.reset_lines(db_id)
            # Process each line of the MPI tasks's output
            while True:
                line = _proc.stdout.readline()
                # EOF
                if not line:
                    break
                stdout_f.write(str(line))
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
        if ((not failed) and (not self.finalize_run(hsh, db_id))):
            failed = True
            failed_comment = 'Could not finalize run for database from hash {} and DB ID {}'.format(
                hsh, db_id)
        # Update the promise
        if failed:
            self._db.fail_promise(promise, failed_comment)
        else:
            self._db.reslove_promise(promise)
            # persist output database info
            # get input database
            input_db = self._db.get_input_db_reverse(hsh, db_id)
            self._db.create_database_info(db_id, "", "", input_db)


    def loop(self):
        """ find all pending running, run forever """
        while True:
            self.wait()
            rows = self._db.get_all_pending_mpi_job()
            for row in rows:
                idx = row[0]
                promise = row[1]
                hsh = row[3]
                try:
                    db_id = self._db.get_db_by_promise(promise)
                    self.run_mpi(promise, hsh, db_id)
                except Exception as err:
                    self._db.fail_mpi_job(promise, err)
                    self.log("Exception during MPI job {}: {}".format(idx, err))
