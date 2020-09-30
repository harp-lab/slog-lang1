import grpc
from concurrent import futures
import time

import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

PORT = 5106

class CompileService(slog_pb2_grpc.LoadProgramService):
    def LoadProgram(self,request,context):
        response = slog.Promise()
        response.success = true
        response.promise_id = 24
        return response

server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))

slog_pb2_grpc.add_LoadProgramServiceServicer_to_server(CompileService(),server)

print('Slog server starting. Listening on port {}'.format(PORT))
server.add_insecure_port('[::]:{}'.format(PORT))
server.start()

try:
    while True:
        time.sleep(86400)
except KeyboardInterrupt:
    server.stop(0)
    print('Server is exiting.')
