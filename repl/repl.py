from concurrent import futures
import grpc
from prompt_toolkit import prompt
from prompt_toolkit.formatted_text import HTML
from random import randint
from sexpdata import loads, dumps
import time
import timeit
from yaspin import yaspin
from repl.parser import *

# Generated protobufs stuff
import protobufs.slog_pb2 as slog_pb2
import protobufs.slog_pb2_grpc as slog_pb2_grpc

# How often to wait between pinging the server
PING_INTERVAL = .1

# Constants (see server.py)
STATUS_PENDING  = 0
STATUS_FAILED   = 1
STATUS_RESOLVED = 2
STATUS_NOSUCHPROMISE = -1

class Repl:
    def __init__(self):
        self._channel = None
        self._parser = CommandParser()
        self.reconnect("localhost")
        self.lasterr = None
        self.i = 0

    def connected(self):
        return self._channel != None
    
    def reconnect(self,server):
        self._server = server
        self._channel = grpc.insecure_channel('{}:5106'.format(server))
        self._stub = slog_pb2_grpc.CommandServiceStub(self._channel)
        self._cur_time = 0
        self.num_local_times = 0
        self.time_prev = {}
        self.time_uuids = {}

    def run(self,filename):
        path = os.path.join(os.getcwd(),filename)
        elaborator = Elaborator()
        try:
            elaborator.elaborate(path)
        except FileNotFoundError as e:
            print("Error processing preambles:")
            print(e)
            return
        # Exchange hashes
        req = slog_pb2.HashesRequest()
        req.session_key = "empty"
        req.hashes.extend(elaborator.hashes.keys())
        response = self._stub.ExchangeHashes(req)
        req = slog_pb2.PutHashesRequest()
        req.session_key = "empty"
        for hsh in response.hashes:
            req.bodies.extend([elaborator.hashes[hsh]])
        response = self._stub.PutHashes(req)

        # Generate a run request
        req = slog_pb2.RunHashesRequest()
        req.hashes.extend(elaborator.hashes.keys())
        response = self._stub.RunHashes(req)
        cmmt = None
        
        # Wait to resolve the promise in the terminal...
        with yaspin(text="Running...") as spinner:
            # Break when promise is resolved
            while True:
                time.sleep(PING_INTERVAL)
                p = slog_pb2.Promise()
                p.promise_id = response.promise_id
                res = self._stub.QueryPromise(p)
                if (cmmt == None and res.err_or_db != ""):
                    cmmt = res.err_or_db
                    spinner.write("✔ {}".format(cmmt))
                elif (cmmt != res.err_or_db):
                    cmmt = res.err_or_db
                    spinner.write("✔ {}".format(cmmt))
                if (res.status == STATUS_PENDING):
                    continue
                elif (res.status == STATUS_FAILED):
                    spinner.fail("💥")
                    print("Execution failed!")
                    print(res.err_or_db)
                    return
                elif (res.status == STATUS_RESOLVED):
                    spinner.ok("✅ ")
                    return

    def get_front(self):
        if (not self.connected()):
            return "Disconnected"
        elif (self._cur_time == 0):
            return "⊥"
        else:
            return self.time

    def loop(self):
        while True:
            try:
                front = self.get_front()
                text = prompt('σλoγ [{}] » '.format(front), bottom_toolbar=self.bottom_toolbar())
                cmd = self._parser.parse(text)
                if cmd:
                    cmd.execute(self)
                else:
                    pass
            except EOFError:
                self.exit()
            except AssertionError:
                return
            
    def exit(self):
        print('Goodbye.')
        exit(0)

    def calc_ping(self):
        req = slog_pb2.PingRequest()
        start_time = timeit.default_timer()
        res = self._stub.Ping(req)
        end_time = timeit.default_timer()
        elapsed = end_time - start_time
        return elapsed * 1000

    def bottom_toolbar(self):
        ping = self.calc_ping()
        if self.connected():
            return HTML('<style color="lightgreen">[host: <b>{}</b> ping: {:.2f} ms]  [?? jobs in queue]</style>'.format(self._server,self.calc_ping(),self.i))
        else:
            return HTML('Disconnected. Use `connect <host>`')

repl = Repl()

try:
    while True:
        repl.loop()
except KeyboardInterrupt:
    repl.exit()
