import grpc
from concurrent import futures
import time
import threading
import sqlite3
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc
import os
import sys
import tempfile
from daemon.compile_task import *

PORT = 5106
DB_PATH = os.path.join(os.path.dirname(__file__),"../metadatabase/database.sqlite3")
DATA_PATH = os.path.join(os.path.dirname(__file__),"../data")
conn = sqlite3.connect(DB_PATH)
log = sys.stderr

class CommandService(slog_pb2_grpc.CommandServiceServicer):
    def __init__(self):
        self._db = conn
        
    def gen_data_directory(self):
        return tempfile.mkdtemp(prefix=DATA_PATH+"/")

    def LoadProgram(self,request,context):
        print("here")
        print(request)
        data_directory = self.gen_data_directory()
        src_directory = os.path.join(data_directory,"src")
        os.mkdir(src_directory,mode=0o700)
        src_file = os.path.join(src_directory,"program.slog")
        print(src_file)
        with open(src_file,"w") as fh:
            fh.write(request.source_program)
        print("Wrote {}\n".format(src_file))
        response = slog_pb2.Promise()
        response.success = True
        response.promise_id = 24
        return response

server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))

slog_pb2_grpc.add_CommandServiceServicer_to_server(CommandService(),server)

def start_compile_task():
    CompileTask(conn,log).loop()

print('Slog server starting. Listening on port {}'.format(PORT))
server.add_insecure_port('[::]:{}'.format(PORT))

# Start the compile task
compile_task = threading.Thread(target=start_compile_task, daemon=True)
compile_task.start()

# Start the server
server.start()

try:
    while True:
        time.sleep(86400)
except KeyboardInterrupt:
    server.stop(0)
    print('Server is exiting.')
