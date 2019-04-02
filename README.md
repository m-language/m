M Language
==========

[The M programming language](https://m-language.github.io/), a minimal Lisp for the JVM, JS, and Native.

Building
--------

Building the compiler requires Java, Kotlin, and Gradle to be installed.
In addition, [the M jvm backend](https://github.com/m-language/m-jvm) 
must be cloned in the same directory (if it is not found, the build 
script will clone it for you).

    clean -- Cleans the M compiler
    build -- Builds the M compiler
    repl  -- Launches the M repl

Before building on Linux, `/mpm-root` must be created as a temporary 
package root. In addition, the build files must be given executable 
permissions.

    sudo mkdir /mpm-root
    sudo chmod 777 /mpm-root
    sudo chmod +x build.sh clean.sh repl.sh

Usage
-----

Depending on the backend, mc may be a jar file, a js file, an 
executable, or any other target language that may change its usage.

    mc                  -- Starts the M repl.
    mc <input>          -- Starts the M repl with an input file preloaded.
    mc <input> <output> -- Compiles input and writes the result to the output directory. 

The input file can be a file or a directory -- if it is a directory, all
child files with the extension `.m` will be loaded recursively.
