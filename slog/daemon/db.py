"""
database Util

CAUTION:
the returned row order may not same as db specification
please read query code before you use these function

Yihao Sun
"""

import sqlite3

from slog.daemon.manifest import Manifest
from slog.daemon.const import STATUS_PENDING, STATUS_RESOLVED, STATUS_FAILED


class MetaDatabase:
    """ a SQLite3 database store all meta infomation about slog server """

    def __init__(self, db_path):
        self.db_path = db_path

    def _db_fetchall(self, sql, args=None):
        """ query operation on sqlite3, this return a result set """
        conn = sqlite3.connect(self.db_path)
        cur = conn.cursor()
        if args:
            res_rows = cur.execute(sql, args).fetchall()
        else:
            res_rows = cur.execute(sql).fetchall()
        conn.close()
        return res_rows

    def _db_fechone(self, sql, args=None):
        """ query operation on sqlite3, this return one result """
        conn = sqlite3.connect(self.db_path)
        cur = conn.cursor()
        if args:
            res_row = cur.execute(sql, args).fetchone()
        else:
            res_row = cur.execute(sql).fetchone()
        conn.close()
        return res_row

    def _db_add(self, sql, args):
        """
        some insert to sqlite3, this will cause a commit, and return
        auto increased id
        """
        conn = sqlite3.Connection(self.db_path)
        cur = conn.cursor()
        cur.execute(sql, args)
        new_id = cur.lastrowid
        conn.commit()
        conn.close()
        return new_id

    def _db_update(self, sql, args):
        """
        update some row in sqlite3, this will casue a commit
        """
        conn = sqlite3.Connection(self.db_path)
        cur = conn.cursor()
        cur.execute(sql, args)
        conn.commit()
        conn.close()

    def load_manifest(self, db_id, manifest_file):
        """ load a manifest file into sqlite using slog database id and given path """
        conn = sqlite3.connect(self.db_path)
        cur = conn.cursor()
        manifest = Manifest(manifest_file)
        for relation in manifest.relations:
            # Ignore non-canonical relations, we don't need to record data for those
            name = relation[0].value()
            arity = relation[1]
            tag = relation[2]
            data_file = relation[5]
            size_file = relation[6]
            # read size file to get number of tuples
            with open(size_file, 'r') as size_f:
                num_tuples = int(size_f.readlines()[1])
            cur.execute('INSERT INTO relations'
                        ' (database_id,name,arity,tag,num_tuples,data_file)'
                        ' VALUES (?,?,?,?,?,?)',
                        (db_id, name, arity, tag, num_tuples, data_file))
        conn.commit()
        conn.close()

    def get_all_database(self):
        """ query all database info """
        res = self._db_fetchall(
            'SELECT database_id,tag_name,user,forked_from'
            ' FROM databases')
        return res

    def get_promise_by_id(self, promise_id):
        """ return a promise line with give id, if empty return None """
        rows = self._db_fetchall(
            'SELECT promise_id,status,comment,database_id'
            ' FROM promises_for_databases'
            ' WHERE promise_id = ?',
            (promise_id,))
        if len(rows) == 0:
            return None
        return rows[0]

    def get_promise_by_db(self, db_id):
        """ return a promise line with give database(no dup), if empty return None """
        rows = self._db_fetchall(
            'SELECT promise_id,status,comment,database_id'
            ' FROM promises_for_databases'
            ' WHERE database_id = ?',
            (db_id,))
        if len(rows) == 0:
            return None
        return rows[0]

    def get_db_by_promise(self, promise_id):
        """ get the db id inside a promise """
        db_row = self._db_fechone(
            'SELECT database_id FROM promises_for_databases'
            ' WHERE promise_id = ?',
            (promise_id,))
        if db_row is not None:
            res = db_row[0]
        else:
            res = None
        return res

    def get_relations_by_db_and_tag(self, db_id, tag):
        """ get a relation row by give database and tag, if not found return None """
        relation_row = self._db_fechone(
            'SELECT name,arity,tag,data_file'
            ' FROM relations'
            ' WHERE database_id = ? and tag = ?',
            (db_id, tag))
        return relation_row

    def get_all_relations_in_db(self, db_id):
        """ return all relation row of a slog database """
        rows = self._db_fetchall(
            'SELECT name,arity,tag,data_file FROM'
            ' relations'
            ' WHERE database_id = ?',
            (db_id,))
        return rows

    def get_relation_tag(self, db_id, rel_name, arity):
        """ return  the tag of a relation (it is determined by) name + arity """
        res = self._db_fechone(
            'SELECT tag FROM relations'
            ' WHERE database_id=? AND name=? AND arity=?',
            (db_id, rel_name, arity))
        if res is not None:
            return res[0]

    def get_all_pending_compile_job(self):
        """ return all pending compiled_job row  """
        rows = self._db_fetchall(
            'SELECT * FROM compile_jobs where STATUS = ?',
            (STATUS_PENDING,))
        return rows

    def get_all_pending_mpi_job(self):
        """ return all pending mpi job row """
        rows = self._db_fetchall(
            'SELECT * FROM mpi_jobs where STATUS = ?',
            (STATUS_PENDING,))
        return rows

    def is_file_hash_exists(self, hsh):
        """ check is a file exists return boolean """
        res = True
        res = self._db_fechone(
            'SELECT hash FROM slog_source_files where hash=?',
            (hsh,))
        if res is None or res == []:
            res = False
        return res

    def is_compiled_before(self, hsh):
        """ check if a file has compiled with some input before """
        res = True
        rows = self._db_fetchall(
            'SELECT out_database_id FROM compile_jobs'
            ' WHERE status=? AND hashes=?',
            (STATUS_RESOLVED, hsh))
        if rows is None or rows == []:
            res = False
        return res

    def save_file_hashes(self, file_hashes: dict):
        """
        save a dict of filename map to file hashes, return the dict contain successfully saved file
         """
        conn = sqlite3.Connection(self.db_path)
        cur = conn.cursor()
        saved_dict = {}
        for fname, hsh in file_hashes.items():
            exist_hash = cur.execute('SELECT hash FROM slog_source_files'
                                     ' where hash=?',
                                     (hsh,)).fetchone()
            if exist_hash is None or exist_hash == []:
                cur.execute('INSERT INTO slog_source_files (hash,filename) VALUES (?,?)',
                            (hsh, fname))
                saved_dict[fname] = hsh
        conn.commit()
        conn.close()
        return saved_dict

    def create_db_promise(self, target_db):
        """ create a new database promise and return the id of inserted row """
        new_id = self._db_add(
            'INSERT INTO promises_for_databases'
            ' (status,comment,database_id,creation_time)'
            ' VALUES (?,?,?,time(\'now\'))',
            (STATUS_PENDING, "Compiling to PRAM IR", target_db))
        return new_id

    def create_compile_job(self, promise_id, joined_hashes, in_db, out_db, buckets):
        """
        create a new compile job for some file + input db
        return id of inserted row
        """
        compile_job_id = self._db_add(
            'INSERT INTO compile_jobs'
            ' (promise, status, hashes, in_database_id, out_database_id, buckets, creation_time)'
            ' VALUES (?,?,?,?,?,?,time(\'now\'))',
            (promise_id, STATUS_PENDING, joined_hashes, in_db, out_db, buckets))
        return compile_job_id

    def create_mpi_job(self, promise_id, in_db, hsh, cores):
        """
        create a mpi job for some database promising
        this job will run with given input database
        and return generated job id
        """
        job_id = self._db_add(
            'INSERT INTO mpi_jobs (promise, status, hash, in_database_id, creation_time, cores)'
            ' VALUES (?,?,?,?,time(\'now\'),?)',
            (promise_id, STATUS_PENDING, hsh, in_db, cores))
        return job_id

    def create_database_info(self, database_id, tag_name, user, forked_from):
        """
        insert info when create a database
        NOTE:
        a database will be created when:
        - compile a program, create a empty database
        - upload a csv, fork form original database
        - run a program generate a output
        """
        self._db_add(
            'INSERT INTO databases (database_id, tag_name, user, forked_from)'
            ' VALUES (?,?,?,?)',
            (database_id, tag_name, user, forked_from))

    def update_promise_comment(self, promise, comment):
        """ update the comment of a promised database """
        self._db_update(
            'UPDATE promises_for_databases'
            ' SET comment = ?'
            ' WHERE promise_id = ?',
            (comment, promise))

    def update_relation_data_info(self, data_file, tuple_num, db_id, rel_name, arity):
        """ update the data file information of a rule """
        self._db_update(
            'UPDATE relations SET num_tuples = ?, data_file = ?'
            ' WHERE database_id = ? AND name = ? AND arity = ?'
            , (tuple_num, data_file, db_id, rel_name, arity))

    def fail_compiled_job(self, promise_id, err_message):
        """ update the compiled job status and error message on a failed promise """
        conn = sqlite3.Connection(self.db_path)
        cur = conn.cursor()
        cur.execute('UPDATE compile_jobs'
                    ' SET status = ?, error = ? WHERE promise = ?',
                    (STATUS_FAILED, err_message, promise_id))
        cur.execute('UPDATE promises_for_databases'
                    ' SET status = ?, comment = ? WHERE promise_id = ?',
                    (STATUS_FAILED, err_message, promise_id))
        conn.commit()
        conn.close()

    def fail_mpi_job(self, promise_id, err_message):
        """ update the compiled job status and error message on a failed promise """
        conn = sqlite3.Connection(self.db_path)
        cur = conn.cursor()
        fail_comment = "Exception during MPI execution." \
                       " Try again or contact administrator for error log."
        cur.execute('UPDATE mpi_jobs'
                    ' SET status = ?, error = ? WHERE promise = ?',
                    (STATUS_FAILED, err_message, promise_id))
        cur.execute('UPDATE promises_for_databases'
                    ' SET status = ?, comment = ? WHERE promise_id = ?',
                    (STATUS_FAILED, fail_comment, promise_id))
        conn.commit()
        conn.close()

    def fail_promise(self, promise_id, comment):
        """ fail a promise """
        self._db_update(
            'UPDATE promises_for_databases'
            ' SET status = ?, comment = ?'
            ' WHERE promise_id = ?',
            (STATUS_FAILED, comment, promise_id))

    def resolve_compiled_job(self, promise_id):
        """ resolve a database promise for a compiled job """
        conn = sqlite3.Connection(self.db_path)
        cur = conn.cursor()
        cur.execute('UPDATE compile_jobs SET status = ? WHERE promise = ?',
                    (STATUS_RESOLVED, promise_id))
        cur.execute('UPDATE promises_for_databases SET status = ? where promise_id = ?',
                    (STATUS_RESOLVED, promise_id))
        conn.commit()
        conn.close()

    def reslove_mpi_job(self, promise_id):
        """ reslove a running program job """
        self._db_update(
            'UPDATE mpi_jobs SET status = ? WHERE promise = ?',
            (STATUS_RESOLVED, promise_id))

    def reslove_promise(self, promise_id):
        """ set a promise into RESOLVED status """
        self._db_update(
            'UPDATE promises_for_databases SET status=? WHERE promise_id=?',
            (STATUS_RESOLVED, promise_id))

    def tag_database(self, database_id, tag):
        """ tag a database in databases table """
        self._db_update(
            'UPDATE databases SET tag_name=? WHERE database_id=?',
            (database_id, tag))
