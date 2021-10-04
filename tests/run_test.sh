#!/usr/bin/env bash
set -e

# Run a test (first argument)
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# go to this scripts location to run tests.
cd $(dirname $(realpath $0))
cd ..

if [ -z $1 ]; then
    echo "Please specify (only) a test name to run."
    exit 1
fi

#echo "Starting server..."
./slog-server &> server_log &

sleep 4

python3 -m tests.$1

echo "Done."
