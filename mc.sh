#!/bin/bash

read -d '' usage <<- EOF
Usage: ./$0 COMMAND                     

COMMAND
    build       Build the compiler
    repl        Build and run the repl
    test        Run compiler tests
    clean       Clean build artifacts

EOF

[ $# -eq 0 ] && { echo "$usage"; exit 1; }

root="./.mpm/"

case "$1" in
    build)
        env MPM_ROOT=$root bash scripts/build.sh
        ;;
    repl)
        env MPM_ROOT=$root bash scripts/run.sh repl
        ;;
    test)
        env MPM_ROOT=$root bash scripts/test.sh
        ;;
    clean)
        env MPM_ROOT=$root bash scripts/clean.sh
        ;;
    *)
        echo "$usage"
        exit 1
esac
