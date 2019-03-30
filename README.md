M-Compiler
==========

The compiler for the M programming language.

Building
--------

Building the compiler requires Java, Kotlin, and Gradle to be installed.
In addition, [the M jvm backend](https://github.com/m-language/m-jvm) 
and [the M standard library](https://github.com/m-language/m-stdlib)
must be cloned in the same directory (if they are not found, the
build script will clone them for you).

    clean -- Cleans the M compiler
    build -- Builds the M compiler
    repl  -- Launches the M repl

Before building on Linux, `/mpm-root` must be created as a 
temporary package root. In addition, the build files must be
given executable permissions.

    sudo mkdir /mpm-root
    sudo chmod 777 /mpm-root
    sudo chmod +x build.sh clean.sh repl.sh

Usage
-----

    mc                  -- Starts the M repl.
    mc <input>          -- Starts the M repl with an input file preloaded.
    mc <input> <output> -- Compiles input and writes the result to the output directory. 
