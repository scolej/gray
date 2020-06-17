export GUILE_LOAD_PATH=$(pwd)

# /usr/bin/guild compile -O3 -o out.go scene-1.scm
# time /usr/bin/guile -c '(load-compiled "out.go")'

time /usr/bin/guile scene-1.scm

# time /usr/bin/guile scene-2.scm
