"""
gRPC server

Kris Micinski
Yihao Sun
"""

import datetime
import hashlib
import math
import os
import shutil
import subprocess
import traceback
import tempfile

from six import MAXSIZE

from daemon.const import DATA_PATH, DATABASE_PATH, CMDSVC_LOG, DB_PATH, SOURCES_PATH, TSV2BIN_PATH
from daemon.const import STATUS_RESOLVED, STATUS_NOSUCHPROMISE, MAX_BUCKETS, MIN_BUCKETS \
                         , MAX_CHUNK_DATA
from daemon.db import MetaDatabase
from daemon.manifest import Manifest

import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc


# util functions
def join_hashes(hashes):
    """ join 2 hash values? """
    return ",".join(hashes)


def generate_db_hash(hashes, using_db=None):
    """
    calculate an output database hash from a combined hash of a list of
    hashes of input files and previous db
    using_db == "" if not using any other DB
    """
    if using_db != "" and using_db is not None:
        hashes = [using_db] + hashes
    hashes.sort()
    cathashes = "|".join(hashes).encode('utf-8')
    hash_func = hashlib.sha256()
    hash_func.update(cathashes)
    return hash_func.hexdigest()


def compute_hash_file(fdata):
    """ generate a hash value from a file content data """
    hash_func = hashlib.sha256()
    hash_func.update(fdata)
    hsh = hash_func.hexdigest()
    return hsh


def read_intern_file(fpath):
    ''' read intern file into a python dict '''
    intern_dict = {}
    with open(fpath, 'r') as intern_file:
        for line in intern_file.readlines():
            if line.strip() != '':
                splited = line.strip().split('\t')
                v_id = splited[0]
                if len(splited) == 1:
                    str_val = ''
                else:
                    str_val = splited[1].strip()
                intern_dict[int(v_id)] = str_val
    return intern_dict


class CommandService(slog_pb2_grpc.CommandServiceServicer):
    """ RPC service that responds to commands from the REPL/etc. See protobufs/slog.proto """

    def __init__(self):
        self._db = MetaDatabase(DB_PATH)

    def log(self, msg):
        out = "[ CommandService {} ] {}".format(
            datetime.datetime.now().strftime("(%H:%M:%S %d/%m/%Y)"), msg)
        print(out)
        CMDSVC_LOG.write(out + "\n")

    def gen_data_directory(self):
        return tempfile.mkdtemp(prefix=DATA_PATH+"/")

    def ExchangeHashes(self, request, context):
        res = slog_pb2.Hashes()
        for h in request.hashes:
            if not self._db.is_file_hash_exists(h):
                res.hashes.extend([h])
        return res

    def Ping(self, request, context):
        return slog_pb2.Pong()

    def QueryPromise(self, request, context):
        res = slog_pb2.PromiseStatus()
        row = self._db.get_promise_by_id(request.promise_id)
        if not row:
            res.status = STATUS_NOSUCHPROMISE
        else:
            res.err_or_db = row[2]
            res.status = row[1]
            if res.status == STATUS_RESOLVED:
                res.err_or_db = row[3]
        return res

    def PutHashes(self, request, context):
        bodies = request.bodies
        ret = slog_pb2.ErrorResponse()
        fdict = {}
        for body in bodies:
            hash_func = hashlib.sha256()
            hash_func.update(body.encode('utf-8'))
            hsh = hash_func.hexdigest()
            fname = os.path.join(SOURCES_PATH, hsh)
            fdict[fname] = hsh
            with open(fname, "w") as slog_code_f:
                slog_code_f.write(body)
                self.log("Writing file for {}".format(hsh))
        self._db.save_file_hashes(fdict)
        return ret

    def PutCSVFacts(self, requests, _context):
        # write csv to tmp
        failed_files = []
        ret = slog_pb2.FactResponse()
        csv_hashes = []
        with tempfile.TemporaryDirectory() as tmp_db_dir:
            # copy original db to tmp_db
            tmp_db_path = os.path.join(tmp_db_dir, '_tmp_db')
            for request in requests:
                body = request.bodies[0].encode('utf-8')
                buckets = request.buckets
                rel_name = request.relation_name
                in_db = request.using_database              # TODO: check all in_db are same
                if not os.path.exists(tmp_db_path):
                    # fork input database
                    shutil.copytree(os.path.join(
                        DATABASE_PATH, in_db), tmp_db_path)
                with tempfile.NamedTemporaryFile() as tmp_csv:
                    tmp_csv.write(body)
                    tmp_csv.seek(0)
                    fst_line = tmp_csv.readline()
                    arity = len(fst_line.decode('utf-8').strip().split('\t'))
                    out_path = os.path.join(tmp_db_path, f'{rel_name}_{arity}')
                    index = ",".join([str(i) for i in range(1, arity+1)])
                    with subprocess.Popen([TSV2BIN_PATH, tmp_csv.name, str(arity), out_path, index,
                                           str(buckets), tmp_db_path],
                                          stdout=subprocess.PIPE, stderr=subprocess.PIPE) as proc:
                        _, err = proc.communicate()
                        if err:
                            ret.success = False
                            failed_files.append(rel_name)
                            print(err)
                        else:
                            ret.success = True
                            csv_hashes.append(compute_hash_file(body))
            if ret.success:
                # persist tmp database
                new_db_id = generate_db_hash(csv_hashes, in_db)
                new_database_path = os.path.join(DATABASE_PATH, new_db_id)
                shutil.copytree(tmp_db_path, new_database_path)
                # update meta db
                # read and update manifest
                mf_path = os.path.join(new_database_path, 'manifest')
                new_mf_s = Manifest(mf_path).generate_mf(new_database_path)
                with open(mf_path, 'w') as _mf:
                    _mf.truncate()
                    _mf.seek(0)
                    _mf.write(new_mf_s)
                self._db.load_manifest(new_db_id, mf_path)
                ret.new_database = new_db_id
                # the default tag name if db_id
                self._db.create_database_info(new_db_id, new_db_id, "", in_db)
        ret.error_msg = ", ". join(failed_files)
        return ret

    def CompileHashes(self, request, context):
        ret = slog_pb2.Promise()
        # Check that all hashes are present
        for hsh in request.hashes:
            if not self._db.is_file_hash_exists(hsh):
                ret.promise_id = -1
                return ret
        using_db = request.using_database
        hashes = set()
        for hsh in request.hashes:
            hashes.add(hsh)
        # In / out DB hashes must be calculated now
        in_db = using_db
        out_db = ""
        # If not using a previous database, we generate an initial DB
        # output is hash of that DB hash + hashes.
        if in_db == "":
            in_db = generate_db_hash(list(hashes))
            out_db = generate_db_hash(list(hashes), in_db)
        # Otherwise output DB is hash of that DB + hashes of others
        else:
            in_db = generate_db_hash(list(hashes), using_db)
            out_db = generate_db_hash(list(hashes), in_db)
        joined_hashes = join_hashes(list(hashes))
        row = self._db.get_promise_by_db(out_db)
        response = slog_pb2.Promise()
        # Already started a compile job for this
        if row is not None:
            response.promise_id = row[0]
            return response
        try:
            # Else, start a compile job and promise the new input DB
            promise_id = self._db.create_db_promise(in_db)
            response.promise_id = promise_id
            buckets = request.buckets
            if (buckets < MIN_BUCKETS or buckets > MAX_BUCKETS):
                self.log(f"Got request for compilation with {buckets} buckets, which is invalid.\n")
                response.promise_id = -1
                return response
            # otherwise, insert, will be handled by CompileTask
            self._db.create_compile_job(promise_id, joined_hashes, in_db, out_db, buckets)
        except Exception as e:
            print(e)
        self.log("Made promise {} for new compile job on hashes {}\n".format(
            promise_id, hashes))
        return response

    def RunHashes(self, request, context):
        ret = slog_pb2.Promise()
        # Check that all hashes are present
        for hsh in request.hashes:
            if not self._db.is_file_hash_exists(hsh):
                ret.promise_id = MAXSIZE
                return ret
        in_db = request.using_database
        hashes = set()
        for hsh in request.hashes:
            hashes.add(hsh)
        # Check that this program was previously successfully compiled
        # using this database
        out_db = self._db.get_compile_out(join_hashes(list(hashes)))
        if out_db is None:
            ret.promise_id = MAXSIZE
            return ret
        row = self._db.get_promise_by_db(out_db)
        response = slog_pb2.Promise()
        # Already started an MPI job for this
        if row is not None:
            response.promise_id = row[0]
            return response
        try:
            # Else, start an MPI job
            promise_id = self._db.create_db_promise(out_db)
            self._db.create_mpi_job(promise_id, in_db)
            response.promise_id = promise_id
        except Exception as e:
            print(e)
        self.log("Made promise {} for new MPI job on hashes {}\n".format(
            promise_id, hashes))
        return response

    def GetTuples(self, request, context):
        row = self._db.get_relations_by_db_and_tag(
            request.database_id, request.tag)
        try:
            arity = int(row[1])
            selection = list(map(int, row[3].split(",")))
            data_file = row[4]
            print(row)
            if data_file.strip() == "":
                response = slog_pb2.Tuples()
                response.status = STATUS_RESOLVED
                response.num_tuples = 0
                response.data.extend([])
                return response
            tuplen = arity+1  # Tuples also include ID column
            mapping = list(range(0, tuplen))
            for selected in selection:
                mapping.remove(selected)
            mapping = selection + mapping
            with open(data_file, 'rb') as rel_data_f:
                file_size = os.stat(data_file).st_size
                num_u64s = int(file_size) / 8
                num_tuples = int(num_u64s) / tuplen
                max_tuples_per_chunk = int(
                    math.floor(MAX_CHUNK_DATA / (8 * arity)))
                num_tuples_left = num_tuples
                while num_tuples_left > 0:
                    num_tuples = int(
                        min(num_tuples_left, max_tuples_per_chunk))
                    buffer = rel_data_f.read(num_tuples*tuplen*8)
                    response = slog_pb2.Tuples()
                    response.status = STATUS_RESOLVED
                    response.num_tuples = num_tuples
                    cpy = [-1 for _ in range(tuplen*num_tuples)]
                    # Shuffle tuples according
                    for row_num in range(num_tuples):
                        for i in range(arity+1):
                            cpy[row_num*tuplen + mapping[i]] = int.from_bytes(
                                buffer[row_num*tuplen*8 + i*8:row_num*tuplen*8 + (i+1)*8], 
                                'little', signed=False)
                    num_tuples_left -= num_tuples
                    response.num_tuples = num_tuples
                    response.data.extend(cpy)
                    yield response
        except Exception as err:
            traceback.print_exc()
            self.log("Err {}".format(err))

    def GetRelations(self, request, context):
        print(request.database_id)
        rows = self._db.get_all_relations_in_db(request.database_id,)
        res = slog_pb2.RelationDescriptionsResponse()
        res.success = True
        for row in rows:
            desc_response = slog_pb2.RelationDescription()
            desc_response.name = row[0]
            desc_response.arity = row[1]
            desc_response.tag = row[2]
            res.relations.extend([desc_response])
        return res

    def GetStrings(self, request, _context):
        string_csv_path = os.path.join(
            DATABASE_PATH, request.database_id, '$strings.csv')
        if os.path.exists(string_csv_path):
            str_dict = read_intern_file(string_csv_path)
            for str_id, str_val in str_dict.items():
                yield slog_pb2.Strings(id=str_id, text=str_val)
        else:
            print(f'string file {string_csv_path} not exists!')

    def ShowDB(self, request, context):
        for db_info_row in self._db.get_all_database():
            db_info_response = slog_pb2.DatabaseInfo()
            db_info_response.database_id = db_info_row[0]
            db_info_response.tag_name = db_info_row[1]
            db_info_response.user = db_info_row[2]
            db_info_response.forked_from = db_info_row[3]
            yield db_info_response
