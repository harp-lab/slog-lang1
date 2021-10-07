"""
This module hosts common functionality that may be useful to 2 or more
other parts of the python-slog codebase
"""

import json
import os
import time

import grpc

from slog.daemon.const import STATUS_PENDING, STATUS_FAILED, STATUS_RESOLVED, STATUS_NOSUCHPROMISE
import slog.protobufs.slog_pb2 as slog_pb2
import slog.protobufs.slog_pb2_grpc as slog_pb2_grpc

# How often to wait between pinging the server
PING_INTERVAL = .1

def rel_name_from_file(fpath):
    """
    Gets the relation name from a fact file
    Currently this is the name of the file itself.
    """
    base = os.path.basename(fpath)
    return base[:base.find('.')]

# TODO: seems unused, get rid of?
def get_arity_souffle_facts(souffle_fpath):
    ''' get arity of a souffle facts file'''
    with open(souffle_fpath, 'r') as souffle_f:
        fst_line = souffle_f.readline()
    if fst_line.strip() == '':
        return -1
    else:
        return len(fst_line.strip().split('\t'))


def make_stub(server_address, max_attempts=5):
    json_config = json.dumps(
        {
            "methodConfig": [
                {
                    "name": [{"service": "<package>.<service>"}],
                    "retryPolicy": {
                        "maxAttempts": max_attempts,
                        "initialBackoff": "0.1s",
                        "maxBackoff": "10s",
                        "backoffMultiplier": 2,
                        "retryableStatusCodes": ["UNAVAILABLE"],
                    },
                }
            ]
        }
    )

    channel = grpc.insecure_channel(server_address, options=[("grpc.service_config", json_config)])
    return channel, slog_pb2_grpc.CommandServiceStub(channel)
