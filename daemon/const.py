"""
constant

Yihao Sun
"""

import os

# Network
PORT = 5108

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
TSV2BIN_PATH = os.path.join(os.path.dirname(__file__), "../parallel-RA/build/tsv_to_bin")

# Logs
CMDSVC_LOG = open(os.path.join(DATA_PATH,"cmdsvc.log"),'a')
COMPILESVC_LOG = open(os.path.join(DATA_PATH,"compilesvc.log"),'a')
RUNSVC_LOG = open(os.path.join(DATA_PATH,"runsvc.log"),'a')

# Statuses
STATUS_PENDING  = 0
STATUS_FAILED   = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = 10

# Compilation timeout in seconds
COMPILATION_TIMEOUT = 20

# Maximum number of bytes allowed per chunk
MAX_CHUNK_DATA = 2097152

MIN_BUCKETS = 1
MAX_BUCKETS = 50000