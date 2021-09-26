# Generated by the gRPC Python protocol compiler plugin. DO NOT EDIT!
"""Client and server classes corresponding to protobuf-defined services."""
import grpc

import slog_pb2 as slog__pb2


class CommandServiceStub(object):
    """Missing associated documentation comment in .proto file."""

    def __init__(self, channel):
        """Constructor.

        Args:
            channel: A grpc.Channel.
        """
        self.ExchangeHashes = channel.unary_unary(
                '/CommandService/ExchangeHashes',
                request_serializer=slog__pb2.HashesRequest.SerializeToString,
                response_deserializer=slog__pb2.Hashes.FromString,
                )
        self.PutHashes = channel.unary_unary(
                '/CommandService/PutHashes',
                request_serializer=slog__pb2.PutHashesRequest.SerializeToString,
                response_deserializer=slog__pb2.ErrorResponse.FromString,
                )
        self.PutCSVFacts = channel.unary_unary(
                '/CommandService/PutCSVFacts',
                request_serializer=slog__pb2.PutCSVFactsRequest.SerializeToString,
                response_deserializer=slog__pb2.ErrorResponse.FromString,
                )
        self.CompileHashes = channel.unary_unary(
                '/CommandService/CompileHashes',
                request_serializer=slog__pb2.CompileHashesRequest.SerializeToString,
                response_deserializer=slog__pb2.Promise.FromString,
                )
        self.Ping = channel.unary_unary(
                '/CommandService/Ping',
                request_serializer=slog__pb2.PingRequest.SerializeToString,
                response_deserializer=slog__pb2.Pong.FromString,
                )
        self.RunHashes = channel.unary_unary(
                '/CommandService/RunHashes',
                request_serializer=slog__pb2.RunProgramRequest.SerializeToString,
                response_deserializer=slog__pb2.Promise.FromString,
                )
        self.QueryPromise = channel.unary_unary(
                '/CommandService/QueryPromise',
                request_serializer=slog__pb2.PromiseRequest.SerializeToString,
                response_deserializer=slog__pb2.PromiseStatus.FromString,
                )
        self.GetRelations = channel.unary_unary(
                '/CommandService/GetRelations',
                request_serializer=slog__pb2.DatabaseRequest.SerializeToString,
                response_deserializer=slog__pb2.RelationDescriptionsResponse.FromString,
                )
        self.GetTuples = channel.unary_stream(
                '/CommandService/GetTuples',
                request_serializer=slog__pb2.RelationRequest.SerializeToString,
                response_deserializer=slog__pb2.Tuples.FromString,
                )
        self.GetStrings = channel.unary_stream(
                '/CommandService/GetStrings',
                request_serializer=slog__pb2.StringRequest.SerializeToString,
                response_deserializer=slog__pb2.Strings.FromString,
                )


class CommandServiceServicer(object):
    """Missing associated documentation comment in .proto file."""

    def ExchangeHashes(self, request, context):
        """Client sends hashes, server returns unstored hashes
        """
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def PutHashes(self, request, context):
        """Put hashes
        """
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def PutCSVFacts(self, request, context):
        """upload a CSV file into slog fact database
        """
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def CompileHashes(self, request, context):
        """Compile hashes, promise for initial DB (immediately resolved if previously compiled)
        """
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def Ping(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def RunHashes(self, request, context):
        """Run a set of hashes with an initial database
        """
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def QueryPromise(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def GetRelations(self, request, context):
        """Get a description of relations for a database
        """
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def GetTuples(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')

    def GetStrings(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')


def add_CommandServiceServicer_to_server(servicer, server):
    rpc_method_handlers = {
            'ExchangeHashes': grpc.unary_unary_rpc_method_handler(
                    servicer.ExchangeHashes,
                    request_deserializer=slog__pb2.HashesRequest.FromString,
                    response_serializer=slog__pb2.Hashes.SerializeToString,
            ),
            'PutHashes': grpc.unary_unary_rpc_method_handler(
                    servicer.PutHashes,
                    request_deserializer=slog__pb2.PutHashesRequest.FromString,
                    response_serializer=slog__pb2.ErrorResponse.SerializeToString,
            ),
            'PutCSVFacts': grpc.unary_unary_rpc_method_handler(
                    servicer.PutCSVFacts,
                    request_deserializer=slog__pb2.PutCSVFactsRequest.FromString,
                    response_serializer=slog__pb2.ErrorResponse.SerializeToString,
            ),
            'CompileHashes': grpc.unary_unary_rpc_method_handler(
                    servicer.CompileHashes,
                    request_deserializer=slog__pb2.CompileHashesRequest.FromString,
                    response_serializer=slog__pb2.Promise.SerializeToString,
            ),
            'Ping': grpc.unary_unary_rpc_method_handler(
                    servicer.Ping,
                    request_deserializer=slog__pb2.PingRequest.FromString,
                    response_serializer=slog__pb2.Pong.SerializeToString,
            ),
            'RunHashes': grpc.unary_unary_rpc_method_handler(
                    servicer.RunHashes,
                    request_deserializer=slog__pb2.RunProgramRequest.FromString,
                    response_serializer=slog__pb2.Promise.SerializeToString,
            ),
            'QueryPromise': grpc.unary_unary_rpc_method_handler(
                    servicer.QueryPromise,
                    request_deserializer=slog__pb2.PromiseRequest.FromString,
                    response_serializer=slog__pb2.PromiseStatus.SerializeToString,
            ),
            'GetRelations': grpc.unary_unary_rpc_method_handler(
                    servicer.GetRelations,
                    request_deserializer=slog__pb2.DatabaseRequest.FromString,
                    response_serializer=slog__pb2.RelationDescriptionsResponse.SerializeToString,
            ),
            'GetTuples': grpc.unary_stream_rpc_method_handler(
                    servicer.GetTuples,
                    request_deserializer=slog__pb2.RelationRequest.FromString,
                    response_serializer=slog__pb2.Tuples.SerializeToString,
            ),
            'GetStrings': grpc.unary_stream_rpc_method_handler(
                    servicer.GetStrings,
                    request_deserializer=slog__pb2.StringRequest.FromString,
                    response_serializer=slog__pb2.Strings.SerializeToString,
            ),
    }
    generic_handler = grpc.method_handlers_generic_handler(
            'CommandService', rpc_method_handlers)
    server.add_generic_rpc_handlers((generic_handler,))


 # This class is part of an EXPERIMENTAL API.
class CommandService(object):
    """Missing associated documentation comment in .proto file."""

    @staticmethod
    def ExchangeHashes(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/ExchangeHashes',
            slog__pb2.HashesRequest.SerializeToString,
            slog__pb2.Hashes.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def PutHashes(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/PutHashes',
            slog__pb2.PutHashesRequest.SerializeToString,
            slog__pb2.ErrorResponse.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def PutCSVFacts(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/PutCSVFacts',
            slog__pb2.PutCSVFactsRequest.SerializeToString,
            slog__pb2.ErrorResponse.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def CompileHashes(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/CompileHashes',
            slog__pb2.CompileHashesRequest.SerializeToString,
            slog__pb2.Promise.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def Ping(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/Ping',
            slog__pb2.PingRequest.SerializeToString,
            slog__pb2.Pong.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def RunHashes(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/RunHashes',
            slog__pb2.RunProgramRequest.SerializeToString,
            slog__pb2.Promise.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def QueryPromise(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/QueryPromise',
            slog__pb2.PromiseRequest.SerializeToString,
            slog__pb2.PromiseStatus.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def GetRelations(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/CommandService/GetRelations',
            slog__pb2.DatabaseRequest.SerializeToString,
            slog__pb2.RelationDescriptionsResponse.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def GetTuples(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_stream(request, target, '/CommandService/GetTuples',
            slog__pb2.RelationRequest.SerializeToString,
            slog__pb2.Tuples.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)

    @staticmethod
    def GetStrings(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_stream(request, target, '/CommandService/GetStrings',
            slog__pb2.StringRequest.SerializeToString,
            slog__pb2.Strings.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)
