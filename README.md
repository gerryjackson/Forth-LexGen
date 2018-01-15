# Forth-LexGen

LexGen generates a lexical analyser (lexer) for scanning and recognising patterns in a text file. It is suitable for compilers or any application requiring that sort of functionality. It is written in ANS Forth/Forth 2012 and was initially released in 2006. The software has been tested on 32 and 64 bit MS Windows 7 and 10, and should run on any ANS Forth/Forth 2012 system - it has been tested with GForth, SwiftForth, VFX Forth, Win32 Forth and BigForth.

The system is fully described in the manual available in this repository at doc/lexgen.pdf.

The output lexer is a Forth program, in principle it is possible to generate a lexer in other languages by modifying one of the source files.

LexGen is compatible with two other Forth tools, Grace and Regex each of which generate files for the others. These other tools will be placed in GitHub in the near future but until then older versions are available, together with the support files for LexGen, in this repository at tools/tools.zip.
