"""
constant

Yihao Sun
"""

import os

# Network
PORT = 5108

# FTP
FTP_ADDR = ('', 2121)
FTP_DEFAULT_USER = "slog_ftp"
FTP_DEFAULT_PWD = "slog_ftp"

# project root dir location
PROJECT_ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))

# Database
DB_PATH = os.path.join(PROJECT_ROOT_DIR, "metadatabase/database.sqlite3")

# Static databse
DATA_PATH = os.path.join(PROJECT_ROOT_DIR, "data")
DATABASE_PATH = os.path.join(DATA_PATH, "databases")
BINS_PATH = os.path.join(DATA_PATH, "binaries")
CMAKE_FILE = os.path.join(DATA_PATH, "binaries/CMakeLists.txt")
SOURCES_PATH = os.path.join(DATA_PATH, "sources")
SLOG_COMPILER_PROCESS = os.path.join(PROJECT_ROOT_DIR, "compiler/slog-process.rkt")
SLOG_COMPILER_ROOT = os.path.join(PROJECT_ROOT_DIR, "compiler")
TSV2BIN_PATH = os.path.join(PROJECT_ROOT_DIR, "backend/build/tsv_to_bin")
FTP_DATA_PATH = os.path.join(DATA_PATH, "ftp")

# Logs
CMDSVC_LOG = open(os.path.join(DATA_PATH, "cmdsvc.log"), 'a')
COMPILESVC_LOG = open(os.path.join(DATA_PATH, "compilesvc.log"), 'a')
RUNSVC_LOG = open(os.path.join(DATA_PATH, "runsvc.log"), 'a')

# Statuses
STATUS_PENDING = 0
STATUS_FAILED = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = 10

# Compilation timeout in seconds
COMPILATION_TIMEOUT = 20

# Maximum number of bytes allowed per chunk
MAX_CHUNK_DATA = 2097152

MIN_BUCKETS = 1
MAX_BUCKETS = 50000
