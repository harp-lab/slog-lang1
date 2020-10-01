CREATE TABLE promises (
       id INTEGER NOT NULL PRIMARY KEY,
       status INTEGER NOT NULL,
       comment TEXT,
       creation_time TEXT NOT NULL
);

CREATE TABLE relations (
       job_id INTEGER NOT NULL,
       relation_id INTEGER NOT NULL,
       relation_arity INTEGER NOT NULL,
       num_tuples INTEGER NOT NULL,
       data_file TEXT NOT NULL,
       PRIMARY KEY(job_id,relation_id,relation_arity)
);

CREATE TABLE string_pools (
       job_id INTEGER NOT NULL,
       pool_type INTEGER NOT NULL,
       pool_file TEXT NOT NULL,
       PRIMARY KEY(job_id,pool_type)
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

CREATE TABLE compile_jobs (
       job_id INTEGER NOT NULL PRIMARY KEY,
       promise INTEGER NOT NULL,
       status INTEGER NOT NULL,
       root_directory TEXT,
       creation_time TEXT NOT NULL,
       completion_time TEXT       
);

