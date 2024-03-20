This folder contains the compiled byte code of an interpreter for the stack
programming language of Assignment 1. See Assignment 1 for more details about
the language.




Description of Files:
---------------------
  - *.cmo
      These files are compiled OCaml bytecode for the interpreter. You will
      need to compile these files into an executable. See below for
      instructions. Compiling these files will produce an executable
      called "interp". See below for instructions on how to execute
      the interpreter.

  - eg*.stack
      These are example files that you can run the interpreter on.


How to compile:
---------------
  You can simply execute "make".

  Alternatively (if you don't have make installed) you can run the
  following command:

    ocamlc -o interp ast.cmo lexer.cmo parser.cmo main.cmo


  After compiling, you should have an executable called "interp".

How to execute:
---------------

  Run the executable on a program: "./interp eg1.stack"

  This will read in the file eg1.stack and interpret it. If your program
  terminates in a reasonable amount of time, the interpreter will output
  the answer. If the program takes too long to execute, the interpreter
  will abort with a Timeout exception.

