# m-compiler

The compiler for the M programming language.

## Building

Building the compiler requires Gradle and Kotlin to be installed.
In addition, [the M jvm backend](https://github.com/m-language/m-jvm) 
and [the M standard library](https://github.com/m-language/m-stdlib)
must be cloned in the same directory (if they are not found, the
build script will clone them for you).

    ./clean -- Cleans the M compiler
    ./build -- Builds the M compiler
    ./repl  -- Launches the M repl