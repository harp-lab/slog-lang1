"""
Slog Backend Daemon

Kris Micinski
Yihao Sun
"""

from concurrent import futures
import threading
import time
import traceback

import grpc
from grpc_interceptor import ServerInterceptor

from slog.daemon.const import DATA_PATH, FTP_DATA_PATH, PORT
from slog.daemon.ftp import SlogFtpSever
from slog.daemon.rpc import CommandService
from slog.daemon.task import CompileTask, RunTask
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc

class ExceptionToStatusInterceptor(ServerInterceptor):
    """
    this a a error handler to expose all error in server, because otherwise
    grpc server will eat all error silently make server too hard to debug
    """
    def intercept(
            self,
            method,
            request,
            context: grpc.ServicerContext,
            _method_name: str,
    ):
        try:
            return method(request, context)
        except Exception as e:
            print(e)
            print(traceback.format_exc())

def start_compile_task():
    CompileTask().loop()


def start_mpirun_task():
    RunTask().loop()


def run():
    """ main entrance """
    # boot ftp
    ftp_server = SlogFtpSever(FTP_DATA_PATH)
    ftp_thread = threading.Thread(target=ftp_server.start, daemon=True)
    ftp_thread.start()
    # boot rpc
    interceptors = [ExceptionToStatusInterceptor()]
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10), interceptors=interceptors)
    slog_pb2_grpc.add_CommandServiceServicer_to_server(CommandService(), server)
    print('Slog server starting. Listening on port {}'.format(PORT))
    server.add_insecure_port('[::]:{}'.format(PORT))
    server.start()
    # Start the compile task
    compile_task = threading.Thread(target=start_compile_task, daemon=True)
    compile_task.start()
    # Start the MPI run task
    mpirun_task = threading.Thread(target=start_mpirun_task, daemon=True)
    mpirun_task.start()
    try:
        while True:
            time.sleep(86400)
    except KeyboardInterrupt:
        server.stop(0)
        print('Server is exiting.')

# Start the server
if __name__ == "__main__":
    run()
