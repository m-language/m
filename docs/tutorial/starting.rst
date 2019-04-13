.. _sect-starting:

***************
Getting Started
***************

Choosing a Backend
==================

M has backends for many platforms, and though they can all compile the M
compiler, some provide interop with their platform that is not available with
other backends. As such, it is recommended that you build using the backend that
you are most comfortable with. If this does not matter to you, the M-JVM backend
is currently the most optimized and most stable.

Setting up MPM
==============

MPM is an acronym for M Package Manager, and must be set up to compile the M
compiler. On Linux, this means creating a folder in your root directory with
all permissions which M can use.

.. code-block:: bash

    sudo mkdir -m 777 /mpm-root

Since Windows' root directory has all permissions by default, the M compiler
will create ``mpm-root`` on its own.

Setting up the M Compiler
=========================

The M compiler does not need any special setup to work; simply clone
`The M Compiler <https://github.com/m-language/m-language>`_ in the directory
of your choice.

.. code-block:: bash

    git clone https://github.com/m-language/m-language.git

Setting up your Editor
======================

There are M plugins for several popular editors:

- `VSCode <https://github.com/m-language/vscode-m>`_
- `Vim <https://github.com/m-language/vim-m>`_
- `IDEA <https://github.com/m-language/intellij-m>`_

If you want to add support for your favorite editor, please 
`create a feature request <https://github.com/m-language/m-language/issues/new?assignees=aedans&labels=&template=feature_request.md&title=%5BFEATURE%5D>`_.

Building M-JVM
==============

Before building M-JVM, you will need:

- Java version 1.8 or higher.
- Kotlin version 1.3 or higher.
- Gradle version 4.4 or higher.

Once these are installed, simply run ``./build`` or ``./build.bat`` to build
the compiler. Then you can start the compiler with ``./mc`` or ``./mc.bat``.

Building M-JS
=============

TODO

Building M-Native
=================

TODO

Using the M Compiler
====================

Once the M compiler is built, it can be ran with the following arguments:

.. code-block:: bash

    mc                  -- Starts the repl.
    mc <input>          -- Compiles <input> and starts the repl with it loaded.
    mc <input> <output> -- Compiles <input> and writes the result to the directory <output>.

The input file can be a file or a directory; if it is a directory, all child
files with the extension ``.m`` will be loaded recursively.
