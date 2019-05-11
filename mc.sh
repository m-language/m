#!/bin/bash

read -d '' usage <<- EOF
Usage: $0 COMMAND                     

COMMAND
    build       Build the compiler
    clean       Clean build artifacts
    rebuild     Same as \`clean && build\`
    repl        Build and run the repl
    test        Run compiler tests

EOF

[ $# -eq 0 ] && { echo "$usage"; exit 1; }

root="./.mpm/"

case "$1" in
    build)
        env MPM_ROOT=$root bash scripts/build.sh
        ;;
    clean)
        env MPM_ROOT=$root bash scripts/clean.sh
        ;;
    rebuild)
        env MPM_ROOT=$root bash scripts/clean.sh
        env MPM_ROOT=$root bash scripts/build.sh 
        ;;
    repl)
        env MPM_ROOT=$root bash scripts/run.sh repl
        ;;
    test)
        env MPM_ROOT=$root bash scripts/test.sh
        ;;
    *)
        echo "$usage"
        exit 1
esac
