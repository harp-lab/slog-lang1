# Run a test (first argument)
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# go to this scripts location to run tests.
cd $(dirname $(realpath $0))
cd ../.. # go to project root

if [ -z $1 ]; then
    echo "Please specify (only) a test name to run."
    exit 1
fi

./slog-server &> server_log &

sleep 4

python3 -m slog.tests.$1

echo "Done."
