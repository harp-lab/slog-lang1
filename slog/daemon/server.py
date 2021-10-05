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

from slog.daemon.const import PORT
from slog.daemon.rpc import CommandService
from slog.daemon.task import CompileTask, RunTask
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc

class ExceptionToStatusInterceptor(ServerInterceptor):
    def intercept(
            self,
            method,
            request,
            context: grpc.ServicerContext,
            _method_name: str,
    ):
        """Override this method to implement a custom interceptor.

         You should call method(request, context) to invoke the
         next handler (either the RPC method implementation, or the
         next interceptor in the list).

         Args:
             method: The next interceptor, or method implementation.
             request: The RPC request, as a protobuf message.
             context: The ServicerContext pass by gRPC to the service.
             method_name: A string of the form
                 "/protobuf.package.Service/Method"

         Returns:
             This should generally return the result of
             method(request, context), which is typically the RPC
             method response, as a protobuf message. The interceptor
             is free to modify this in some way, however.
         """
        try:
            return method(request, context)
        except Exception as e:
            print(e)
            print(traceback.format_exc())

def start_compile_task():
    CompileTask().loop()


def start_mpirun_task():
    RunTask().loop()


# Start the server
if __name__ == "__main__":
    interceptors = [ExceptionToStatusInterceptor()]
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10), interceptors=interceptors)
    slog_pb2_grpc.add_CommandServiceServicer_to_server(
        CommandService(), server)
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
