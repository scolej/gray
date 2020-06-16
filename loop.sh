# Trap to ensure we kill all subprocesses when we interrupt the loop.
function stop() {
    kill -9 -$PID
    exit -1
}
trap stop SIGINT

while true
do
    # Run script with new process group so we can kill everthing at once.
    setsid sh wip.sh &
    PID=$!

    find -name \*.scm > watch
    echo ./wip.sh >> watch
    inotifywait -e modify --fromfile watch
    kill -9 -$PID

    # Wait a moment for any other files to be written.
    # `inotifywait` debouce?
    sleep 0.5
done;
