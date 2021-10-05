"""
These are common 'verbs' for things to do
"""

import copy
import os
import time

from six import MAXSIZE

from slog.common import rel_name_from_file, run_until_promised
from slog.common.elaborator import Elaborator
from slog.daemon.const import STATUS_PENDING, STATUS_FAILED, STATUS_RESOLVED, COMPILATION_TIMEOUT
import slog.protobufs.slog_pb2 as slog_pb2


def compile_slog(stub, cur_db, slogfile, spinner=None):
    """
    Takes a gRPC stub, the current DB hash, and a path to a .slog file
    will send the file to the server and compile it.
    The function returns the new DB hash and the compiled program hashes.
    If there is some issue (i.e. compile failure), return False.
    """

    elaborator = Elaborator()
    try:
        elaborator.elaborate(slogfile)
    except FileNotFoundError as e:
        if spinner:
            spinner.write("File not found to compile.")
            spinner.write(e)
        return None
    program_hashes = elaborator.hashes.keys()

    hashreq = slog_pb2.HashesRequest()
    hashreq.session_key = "empty"
    hashreq.hashes.extend(program_hashes)
    hashres = stub.ExchangeHashes(hashreq)

    puthashreq = slog_pb2.PutHashesRequest()
    puthashreq.session_key = "empty"
    for hsh in hashres.hashes:
        puthashreq.bodies.extend([elaborator.hashes[hsh]])
    stub.PutHashes(puthashreq)

    comphashreq = slog_pb2.CompileHashesRequest()
    comphashreq.buckets = 4096
    comphashreq.using_database = cur_db
    comphashreq.hashes.extend(program_hashes)
    comphashres = stub.CompileHashes(comphashreq)

    res = None
    times = COMPILATION_TIMEOUT
    while times > 0:
        RESOLUTION = 1 # wait this many seconds til we check status
        times -= RESOLUTION
        time.sleep(RESOLUTION)
        p = slog_pb2.Promise()
        p.promise_id = comphashres.promise_id
        res = stub.QueryPromise(p)
        if res.status == STATUS_PENDING:
            # if spinner:
            #     spinner.write(f"Pending {res}")
            continue
        elif res.status == STATUS_FAILED:
            if spinner:
                spinner.write(f"Compilation failed! {res}")
            return None
        elif res.status == STATUS_RESOLVED:
            if spinner:
                spinner.write(f"Compilation successful: {res.err_or_db}")
            return res.err_or_db, elaborator.hashes.keys()
    # TODO: is there a cancel-compile request I can make? Yihao says no :(
    if spinner:
        spinner.write(f"Compilation timeout! {res}")
    return None


def upload_csvs(stub, cur_db, csv_dir, spinner=None):
    """
    Uploads CSVs to create a new DB on top of cur_db.
    csv_dir can be a directory of `.facts` files or a single `.facts` file.
    optional argument relations is a set/list of relations to search through for correctness.
    Returns None on some error, and the created DB hash on success
    """
    ''' upload csv to current EDB using tsv_bin tool '''
    def csv_request_generator(csv_file_paths: list):
        ''' a generator to create gRPC stream from a list of facts file '''
        for csv_fname in csv_file_paths:
            rel_name = rel_name_from_file(csv_fname)
            req = slog_pb2.PutCSVFactsRequest()
            req.using_database = cur_db
            req.relation_name = rel_name
            req.buckets = 16
            with open(csv_fname, 'r') as csv_f:
                csv_text = csv_f.read()
                req.bodies.extend([csv_text])
            yield req

    csv_dir = csv_dir.strip()
    csv_file_paths = []
    relations = load_relations(stub, cur_db)
    if not os.path.exists(csv_dir):
        if spinner:
            spinner.write("Fact directory/file does not exist!")
        return None
    if os.path.isdir(csv_dir):
        for fname in os.listdir(csv_dir):
            if not fname.endswith('.facts'):
                continue
            csv_file_paths.append(f'{csv_dir}/{fname}')
    elif csv_dir.endswith('.facts'):
        rel_name = rel_name_from_file(csv_dir)
        # TODO: should we just do what the REPL does and search the DB before hand for relations?
        if rel_name not in relations:
            if spinner:
                spinner.write(f"current database don't have relation {rel_name}" \
                               " please make sure the fact file has name `<rel_name>.facts`")
            return None
        csv_file_paths.append(csv_dir)
    if csv_file_paths == []:
        if spinner:
            spinner.write("NOTE: no valid fact files found! "
                          "Fact files must have extension `.facts`. Continuing.")

    response = stub.PutCSVFacts(csv_request_generator(csv_file_paths))
    if not response.success:
        if spinner:
            spinner.fail("💥")
            spinner.write(f" {response.error_msg} fail to update!")
        return None

    new_db = response.new_database
    if spinner:
        spinner.ok("✅ ")
        spinner.write(f"All relation uploaded. now in database {new_db}")

    return new_db


def run_hashes(stub, cur_db, program_hashes, cores, spinner=None):
    """
    Takes a gRPC stub, the current DB hash,
    and the program hashes of the .slog file.
    Runs the .slog file related to the program hashes on the given DB.
    The output DB hash is returned.
    returns None on some error and the created DB hash on success
    """
    req = slog_pb2.RunProgramRequest()
    req.using_database = cur_db
    req.cores = cores
    req.hashes.extend(program_hashes)
    # Get a promise for the running response
    response = stub.RunHashes(req)
    if spinner:
        spinner.write(f"running promise: {response.promise_id}")
    if response.promise_id == MAXSIZE:
        if spinner:
            spinner.write("running a file that was never loaded!")
        new_db = None
    else:
        new_db = run_until_promised(stub, response.promise_id, spinner)
        if not new_db:
            spinner.write("Execution failed!")
            new_db = None # ensure its exactly None, and not just falsy.
    return new_db

def load_relations(stub, db_id):
    """ get all relation inside a database """
    req = slog_pb2.DatabaseRequest()
    req.database_id = db_id
    res = stub.GetRelations(req)
    relations = []
    for relation in res.relations:
        relations.append([relation.name, relation.arity, relation.tag])
    return relations

def lookup_rels(stub, db_id, name, relations=None):
    """
    Check if a relation info is in the relations
    If relations are not provided, they are loaded.
    """
    if not relations:
        relations = load_relations(stub, db_id)
    return [rel for rel in relations if rel[0] == name]


def get_intern_strings(stub, db_id):
    """ update cached string.csv data """
    req = slog_pb2.StringRequest()
    req.database_id = db_id
    return {sres.id: sres.text for sres in stub.GetStrings(req)}

def dump_relation(stub, cur_db, name, out_file=None):
    """
    Dump a relation of a given name with a given DB.
    Dumps to stdout unless out_file is given, then it is written to a file
    at that path.
    """
    def fetch_tuples(stub, cur_db, name):
        """ print all tulple of a relation """
        req = slog_pb2.RelationRequest()
        req.database_id = cur_db
        looked_up = lookup_rels(stub, cur_db, name)
        arity = looked_up[0][1]
        req.tag = looked_up[0][2]
        row_count = 0
        col_count = 0
        tuples = []
        buf = [-1] * (arity+1)
        interned_strings = get_intern_strings(stub, cur_db)
        for response in stub.GetTuples(req):
            if response.num_tuples == 0:
                continue
            for u64 in response.data:
                if col_count == 0:
                    # index col
                    # rel_tag = u64 >> 46
                    bucket_id = (u64 & BUCKET_MASK) >> 28
                    tuple_id = u64 & (~TUPLE_ID_MASK)
                    buf[0] = (bucket_id, tuple_id, row_count)
                    col_count += 1
                    continue
                val_tag = u64 >> 46
                if val_tag == INT_TAG:
                    attr_val = u64 & VAL_MASK
                elif val_tag == STRING_TAG:
                    attr_val = interned_strings[u64 & VAL_MASK]
                else:
                    # relation
                    rel_name = self.lookup_rel_by_tag(val_tag)[0]
                    # attr_val = f'rel_{rel_name}_{u64 & (~TUPLE_ID_MASK)}'
                    if name != rel_name and rel_name not in self.updated_tuples.keys():
                        self.fetch_tuples(rel_name)
                    bucket_id = (u64 & BUCKET_MASK) >> 28
                    tuple_id = u64 & (~TUPLE_ID_MASK)
                    attr_val = ['NESTED', rel_name, (bucket_id, tuple_id)]
                buf[col_count] = attr_val
                col_count += 1
                if col_count == arity + 1:
                    # don't print id col
                    # rel name at last
                    tuples.append(copy.copy(buf)+[name])
                    col_count = 0
                    row_count += 1
            assert row_count == response.num_tuples
        self.updated_tuples[name] = tuples
        return tuples

    def recursive_dump_tuples(self, rel, out_path):
        """ recursive print all tuples of a relation """
        # reset all tuples to non-updated
        def find_val_by_id(name, row_id):
            for row in self.updated_tuples[name]:
                if row[0] == row_id:
                    return row
            return None
        resolved_relname = []
        def _resolve(rname):
            if rname in resolved_relname:
                return
            for i, row in enumerate(self.updated_tuples[rname]):
                for j, col in enumerate(row[:-1]):
                    if not isinstance(col, list):
                        continue
                    if col[0] == 'NESTED':
                        nested_name = col[1]
                        nested_id = col[2]
                        val = find_val_by_id(nested_name, nested_id)
                        if val is None:
                            val = f'"{nested_name} has no fact with id {nested_id} !"'
                        _resolve(nested_name)
                        self.updated_tuples[rname][i][j] = val
            resolved_relname.append(rname)

        def rel_to_str(rel):
            res = []
            for col in rel[:-1]:
                if isinstance(col, type):
                    if col[0] == 'NESTED':
                        res.append(f"({' '.join([str(v) for v in col])})")
                    else:
                        res.append(rel_to_str(col))
                else:
                    res.append(str(col))
            return f"({rel[-1]} {' '.join(res)})"
        self.fetch_tuples(rel[0])
        # print(self.updated_tuples)
        _resolve(rel[0])
        if not out_path:
            for fact_row in sorted(self.updated_tuples[rel[0]], key=lambda t: int(t[0][2])):
                print(f"#{fact_row[0][2]}:  {rel_to_str(fact_row[1:])}")
        else:
            with open(out_path, 'w') as out_f:
                for fact_row in sorted(self.updated_tuples[rel[0]], key=lambda t: int(t[0][2])):
                    out_f.write(f"#{fact_row[0][2]}:  {rel_to_str(fact_row[1:])}")

    looked_up = lookup_rels(stub, cur_db, name)
    if len(looked_up) == 0:
        print("No relation named {} in the current database".format(name))
    elif len(looked_up) > 1:
        print(f"More than one arity for {name}, not currently"
              " supporting printing for multi-arity relations")
    else:
        self.recursive_dump_tuples(looked_up[0], out_file)


