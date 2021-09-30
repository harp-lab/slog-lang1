"""
database Util

CAUTION:
the returned row order may not same as db specification
please read query code before you use these function

Yihao Sun
"""

import sqlite3

from daemon.manifest import Manifest
from daemon.const import STATUS_PENDING, STATUS_RESOLVED, STATUS_FAILED


class MetaDatabase:
    """ a SQLite3 database store all meta infomation about slog server """

    def __init__(self, db_path):
        self.db_path = db_path

    def load_manifest(self, db_id, manifest_file):
        """ load a manifest file into sqlite using slog database id and given path """
        conn = sqlite3.connect(self.db_path)
        c = conn.cursor()
        manifest = Manifest(manifest_file)
        for relation in manifest.relations:
            # Ignore non-canonical relations, we don't need to record data for those
            pcs = list(map(str, relation[4]))
            name = relation[0].value()
            if name.startswith('$'):
                # inter relation
                continue
            arity = relation[1]
            c.execute('INSERT INTO canonical_relations'
                      ' (database_id,name,arity,selection,tag,num_tuples)'
                      ' VALUES (?,?,?,?,?,0)',
                      (db_id, name, arity, ",".join(pcs), str(relation[3])))
        conn.commit()
        conn.close()

    def get_all_database(self):
        """ query all database info """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        res = c.execute('SELECT database_id,tag_name,user,forked_from'
                        ' FROM databases').fetchall()
        conn.close()
        return res

    def get_promise_by_id(self, promise_id):
        """ return a promise line with give id, if empty return None """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('SELECT promise_id,status,comment,database_id'
                  ' FROM promises_for_databases'
                  ' WHERE promise_id = ?',
                  (promise_id,))
        rows = c.fetchall()
        if len(rows) == 0:
            return None
        conn.close()
        return rows[0]

    def get_promise_by_db(self, db_id):
        """ return a promise line with give database(no dup), if empty return None """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('SELECT promise_id,status,comment,database_id'
                  ' FROM promises_for_databases'
                  ' WHERE database_id = ?',
                  (db_id,))
        rows = c.fetchall()
        if len(rows) == 0:
            return None
        conn.close()
        return rows[0]

    def get_db_by_promise(self, promise_id):
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('SELECT database_id FROM promises_for_databases'
                  ' WHERE promise_id = ?',
                  (promise_id,))
        db_row = c.fetchone()
        if db_row is not None:
            res = db_row[0]
        else:
            res = None
        conn.close()
        return res

    def get_relations_by_db_and_tag(self, db_id, tag):
        """ get a relation row by give database and tag, if not found return None """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        r = c.execute(
            'SELECT name,arity,tag,selection,data_file'
            ' FROM canonical_relations'
            ' WHERE database_id = ? AND tag = ?',
            (db_id, tag)).fetchone()
        conn.close()
        return r

    def get_all_relations_in_db(self, db_id):
        """ return all relation row of a slog database """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute(
            'SELECT name,arity,tag,selection FROM'
            ' canonical_relations'
            ' WHERE database_id = ?',
            (db_id,))
        rows = c.fetchall()
        conn.close()
        return rows

    def get_all_pending_compile_job(self):
        """ return all pending compiled_job row  """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('SELECT * FROM compile_jobs where STATUS = ?',(STATUS_PENDING,))
        rows = c.fetchall()
        conn.close()
        return rows

    def get_all_pending_mpi_job(self):
        """ return all pending mpi job row """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('SELECT * FROM mpi_jobs where STATUS = ?',(STATUS_PENDING,))
        rows = c.fetchall()
        conn.close()
        return rows

    def is_file_hash_exists(self, hsh):
        """ check is a file exists return boolean """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        res = True
        r = c.execute('SELECT hash FROM slog_source_files where hash=?', (hsh,)).fetchone()
        if r is None or r == []:
            res = False
        conn.close()
        return res

    def is_compiled_before(self, hsh):
        """ check if a file has compiled with some input before """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        res = True
        row = c.execute('SELECT out_database_id FROM compile_jobs'
                        ' WHERE status=? AND hashes=?',
                        (STATUS_RESOLVED, hsh)).fetchall()
        if row is None or row == []:
            res = False
        return res

    def save_file_hashes(self, file_hashes: dict):
        """
        save a dict of filename map to file hashes, return the dict contain successfully saved file
         """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        saved_dict = {}
        for fname, hsh in file_hashes.items():
            r = c.execute(
                'SELECT hash FROM slog_source_files where hash=?', (hsh,)).fetchone()
            if r is None or r == []:
                c.execute(
                    'INSERT INTO slog_source_files (hash,filename) VALUES (?,?)',
                    (hsh, fname))
                saved_dict[fname] = hsh
        conn.commit()
        conn.close()
        return saved_dict

    def create_db_promise(self, target_db):
        """ create a new database promise and return the id of inserted row """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('INSERT INTO promises_for_databases'
                  ' (status,comment,database_id,creation_time)'
                  ' VALUES (?,?,?,time(\'now\'))',
                  (STATUS_PENDING, "Compiling to PRAM IR", target_db))
        new_id = c.lastrowid
        conn.commit()
        conn.close()
        return new_id

    def create_compile_job(self, promise_id, joined_hashes, in_db, out_db, buckets):
        """
        create a new compile job for some file + input db
        return id of inserted row
        """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute(
            'INSERT INTO compile_jobs'
            ' (promise, status, hashes, in_database_id, out_database_id, buckets, creation_time)'
            ' VALUES (?,?,?,?,?,?,time(\'now\'))',
            (promise_id, STATUS_PENDING, joined_hashes, in_db, out_db, buckets))
        compile_job_id = c.lastrowid
        conn.commit()
        conn.close()
        return compile_job_id

    def create_mpi_job(self, promise_id, in_db, hsh):
        """
        create a mpi job for some database promising
        this job will run with given input database
        and return generated job id
        """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute(
            'INSERT INTO mpi_jobs (promise, status, hash, in_database_id, creation_time)'
            ' VALUES (?,?,?,time(\'now\'))',
            (promise_id, STATUS_PENDING, hsh, in_db))
        job_id = c.lastrowid
        conn.commit()
        conn.close()
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
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('INSERT INTO databases (database_id, tag_name, user, forked_from)'
                  ' VALUES (?,?,?,?)',
                  (database_id, tag_name, user, forked_from))
        conn.commit()
        conn.close()

    def update_promise_comment(self, promise, comment):
        """ update the comment of a promised database """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute(
            'UPDATE promises_for_databases'
            ' SET comment = ?'
            ' WHERE promise_id = ?',
            (comment,promise))
        conn.commit()
        conn.close()

    def update_relation_data_info(self, data_file, tuple_num, db_id, rel_name, arity):
        """ update the data file information of a rule """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('UPDATE canonical_relations SET num_tuples = ?, data_file = ?'
                  ' WHERE database_id = ? AND name = ? AND arity = ?'
                  ,(tuple_num, data_file, db_id, rel_name, arity))
        conn.commit()
        conn.close()

    def fail_compiled_job(self, promise_id, err_message):
        """ update the compiled job status and error message on a failed promise """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('UPDATE compile_jobs'
                  ' SET status = ?, error = ? WHERE promise = ?',
                  (STATUS_FAILED,err_message,promise_id))
        c.execute('UPDATE promises_for_databases'
                  ' SET status = ?, comment = ? WHERE promise_id = ?',
                  (STATUS_FAILED,err_message,promise_id))
        conn.commit()
        conn.close()

    def fail_mpi_job(self, promise_id, err_message):
        """ update the compiled job status and error message on a failed promise """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        s = "Exception during MPI execution." \
            " Try again or contact administrator for error log."
        c.execute('UPDATE mpi_jobs'
                  ' SET status = ?, error = ? WHERE promise = ?',
                  (STATUS_FAILED,err_message,promise_id))
        c.execute('UPDATE promises_for_databases'
                  ' SET status = ?, comment = ? WHERE promise_id = ?',
                  (STATUS_FAILED,s,promise_id))
        conn.commit()
        conn.close()

    def fail_promise(self, promise_id, comment):
        """ fail a promise """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('UPDATE promises_for_databases SET status = ?, comment = ? WHERE promise_id = ?',
                  (STATUS_FAILED, comment, promise_id))
        conn.commit()
        conn.close()

    def resolve_compiled_job(self, promise_id):
        """ resolve a database promise for a compiled job """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('UPDATE compile_jobs SET status = ? WHERE promise = ?',
                  (STATUS_RESOLVED,promise_id))
        c.execute('UPDATE promises_for_databases SET status = ? where promise_id = ?',
                  (STATUS_RESOLVED,promise_id))
        conn.commit()
        conn.close()

    def reslove_mpi_job(self, promise_id):
        """ reslove a running program job """
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('UPDATE mpi_jobs SET status = ? WHERE promise = ?',
                  (STATUS_RESOLVED, promise_id))
        conn.commit()
        conn.close()

    def reslove_promise(self, promise_id):
        conn = sqlite3.Connection(self.db_path)
        c = conn.cursor()
        c.execute('UPDATE promises_for_databases SET status = ? WHERE promise_id = ?',
                  (STATUS_RESOLVED, promise_id))
        conn.commit()
        conn.close()
