while true
do
    find -name \*.scm > watch

    sh wip.sh &
    PID=$!

    inotifywait -e modify --fromfile watch
    kill -9 $PID
done;
