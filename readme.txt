README FOR BLACK PHANTOM'S C COMPILER
=====================================

This file contains release notes for Black Phantom's C compiler preliminary version.


1. FILES IN THIS PACKAGE
------------------------

This package contains ZIPed development directory. The files include:


- Source files and headers necessary to build the BPC compiler

LEXER.H
PARSER.H
BPC.C
CODEGEN.C
DECLARATIONS.C
LEXER.C
PARSER.C
PPROC.C
STATEMENTS.C
SYMBOLS.C

- The final draft of the current ISO C standard 9899:1999 (C99), which was used as reference for C semantics definition:

DOC/C99.PDF

Sample C source files that demonstrate use of BPC compiler:

SAMPLES\HELLO1.C
SAMPLES\USE_IF.C
SAMPLES\USE_IF2.C
SAMPLES\USE_ARRAY1.C
SAMPLES\BUBBLE_SORT.C
SAMPLES\USE_SCANF.C
SAMPLES\USE_SWITCH.C
SAMPLES\USE_STRUCT.C
SAMPLES\USE_POINTER_ARITH.C
SAMPLES\USE_POINTERS.C
SAMPLES\USE_BREAK.C

- MSDEV project file

BPC\bpc.dsw

- Pre-built executable files and MASM 6.15 assembler.

BIN\bpc.exe
BIN\ml.exe
BIN\ml.err


2. HOW TO BUILD THE COMPILER
----------------------------

The easiest way to build the compiler is to create a project in MSDEV and add all the source files listed above. Then choose 'Rebuild All' option from MSDEV IDE menu. Source file and other command-line options are accepted through the Project->Settings->Debug menu.

Of course, everything can be compiled with a simple makefile. No special options are required; LINK target shall be "Win32 console executable".

BPC is portable. It's untested, but it should be no problem to compile it with any other ISO/ANSI C compiler. The project was built and tested with MSDEV 6.0. It was verified that it doesn't work when compiled with MSDEV 4.0


3. HOW TO COMPILE FILES
-----------------------

BPC is a Win32 console executable. It receives parameters through the command line and yields the intermediate files and resulting assembly source file.

"bpc -h" displays help line.

In order to compile headers directory should be set with the -I option. Microsoft implementation requires the following macros to be defined:

_WIN32
__STDC__ to value 1.

Typical command line that was used during testing is:

bpc -I$(MSDEV_DIR)\include -D_WIN32 -D__STDC__=1 <source_file>

After C source files are the resulting assembly files must be compiled and object files - linked. During tests it was accomplished with the following setup:

- Copy ML.EXE and ML.ERR from the package's BIN\ directory to $(MSDEV_DIR)\BIN
- create a command-line prompt
- run $(MSDEV_DIR)\bin\vcvars32.bat in it. This will set include, lib and path environment variables for MSVC 6.0. 
- assemble the resulting assembly source file:

ml /c /coff <asm_source_file>

- link the resulted .OBJ file with:

link <object_file> libc.lib

- Run the resulting executable from the command line.


4. CURRENT DEVELOPMENT STATUS
-----------------------------

The current status is preliminary alpha. As the commercial project it should be considered as development entering the heavy testing phase. Currently BPC may have unpredicted behavior with some semantic constructs, misbehave when fed erroneous C program as input or produce false code. Additionally, bitfields structure members are not implemented yet.

BPC needs a month-two of thorough testing to reach commercial quality. However, considering that it was written mostly between 12 and 4 hours at night I consider it adequate as of compilers construction course level of homeworks and final project.

The BPC project currently counts 10 source files and 11,500 lines of code (~300K).
 