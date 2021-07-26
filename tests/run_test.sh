# Run a test (first argument)
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
PYTHON=python3
HERE=$PWD

source ~/.profile

if [ -z $1 ]; then
    echo "Please specify (only) a test name to run."
    exit 1
fi

#echo "Starting server..."
cd ..; ./slog-server &>$HERE/server_log &
SERVER=$! # Get server's process ID
sleep 1
cd $HERE
# Run the test
cd ..
#echo "Running test..."
$PYTHON -m tests.$1
#echo "Done."
