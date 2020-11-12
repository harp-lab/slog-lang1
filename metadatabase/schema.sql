CREATE TABLE promises (
       id INTEGER NOT NULL PRIMARY KEY,
       status INTEGER NOT NULL,
       comment TEXT,
       creation_time TEXT NOT NULL
);

/* Associates promises with their eventual (if we don't crash) databases. */
CREATE TABLE promises_for_databases (
       promise_id INTEGER NOT NULL PRIMARY KEY,
       /* a hash */
       database_id TEXT NOT NULL
);

CREATE TABLE canonical_relations (
       /* hash of the database ID */
       database_id TEXT NOT NULL,
       /* relation name */
       name TEXT NOT NULL,
       /* relation arity */
       arity INTEGER NOT NULL,
       /* selection in comma-separated format, e.g., `3,0,5` */
       selection TEXT NOT NULL,
       /* 16-bit tag specific to rel-arity and identifying this database */
       tag INTEGER NOT NULL,
       /* Note: file on disc holding the contents of this relation is assumed by convention */
       PRIMARY KEY(database_id,name,arity)
);
       
CREATE TABLE string_pools (
       job_id INTEGER NOT NULL,
       pool_type INTEGER NOT NULL,
       pool_file TEXT NOT NULL,
       PRIMARY KEY(job_id,pool_type)
);

CREATE TABLE slog_source_files (
       hash TEXT NOT NULL PRIMARY KEY,
       filename TEXT NOT NULL
);

CREATE TABLE users (
       user_id INTEGER NOT NULL PRIMARY KEY,
       username TEXT NOT NULL,
       hashed_salted_pw TEXT NOT NULL
);

CREATE TABLE sessions (
       session_id INTEGER NOT NULL PRIMARY KEY,
       user_id INTEGER NOT NULL,
       creation_time TEXT NOT NULL
);

CREATE TABLE mpi_jobs (
       job_id INTEGER NOT NULL PRIMARY KEY,
       promise INTEGER NOT NULL,
       status INTEGER NOT NULL,
       hash TEXT NOT NULL,
       creation_time TEXT NOT NULL,
       completion_time TEXT
);
 
CREATE TABLE compile_jobs (
       job_id INTEGER NOT NULL PRIMARY KEY,
       promise INTEGER NOT NULL,
       status INTEGER NOT NULL,
       hashes TEXT NOT NULL,
       creation_time TEXT NOT NULL,
       completion_time TEXT,
       error TEXT
);

