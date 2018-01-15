\ LexGen - Save state transition tables module - save lex tables to file

\ Copyright (C) Gerry Jackson 2006, 2008, 2010, 2011, 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

[defined] [-dev-] [if] .( SaveTables.fth loading ...) cr [then]

\ ---[ Output file handling ]---------------------------------------------------

variable lexFile 0 lexFile !				\ file identifier

: createFile	( caddr u -- )
	w/o create-file				( -- fid ior )
	abort" Unable to create output file"
	lexFile !
;

: closeFile		( -- )
	lexFile @ close-file					( -- ior )
	abort" Unable to close output file"
;

\ ---[ Definitions to write to file ]-------------------------------------------

: writeText ( caddr u -- )
   lexFile @ write-file abort" Writing text to file failed"
;

: writeLine ( caddr u -- )
   lexFile @ write-line abort" Writing a line of text to file failed"
;

: writeNewLine    ( -- ) 0 0 writeLine ;

: writeSpace		( -- ) s"  " writeText ;

: writeSpaces     ( n -- ) 0 max 0 ?do writeSpace loop ;

: writeNumber.R   ( n u -- )
   over 2>r abs 0 <# #s r> sign #>
   r> over - 0 max writeSpaces writeText
;

: writeNumber		( n -- ) 0 writeNumber.R ;

variable llen  0 llen !    \ Cumulative length of the current line
80 constant max-llen

: ?llen  ( u -- f ) llen @ + max-llen > ;  \ Returns true if line too long

-1 value ugly     \ Generates an unformatted state transition table
: formatted  0 to ugly ;   \ User can force formatted output

: write-nl       writeNewLine 0 llen ! ;
: write-newline  ugly 0= if write-nl then ;
: write-text  ( caddr u -- )  dup llen +! writeText ;
: write-spaced-text  ( caddr u -- )  WriteSpace 1 llen +! write-text ;   

\ write-string is the word to call to write text to the file
: write-string  ( caddr u -- )
   dup 1+ ?llen
   if write-nl then
   llen @
   if write-spaced-text else write-text then
;

: write-stringline  ( caddr u )
   write-string write-newline
;

: ?write-stringline  ( caddr u -- )
   ugly if 2drop else write-stringline then
;

: write-string.R  ( caddr u n2 -- )
   ugly if drop write-string else over - writeSpaces write-string then
;

: n>string  ( n -- caddr u )  \ Signed
   dup >r abs 0 <# #s r> sign #>
;

: write-number  ( n -- )  n>string write-string ;

: write-number.R  ( n1 n2 -- )
   ugly if drop write-number else writeNumber.R then
;

: ensure-space  ( u1 caddr2 u2 -- caddr2 u2 )  \ Keep together e.g. ~ 123
   rot over + 2 + ?llen
   if write-nl then
;

: append-file  ( caddr u -- )
   r/o open-file throw dup >r      ( -- fid ) ( R: -- fid )
   file-size throw                     ( -- ud )
   abort" File too large"              ( -- u2 )
   dup allocate throw swap 2dup        ( -- caddr2 u2 caddr2 u2 )
   r@ read-file throw                  ( -- caddr2 u2 u3 )
   nip
   r> close-file throw
   2dup writeText drop free throw      ( -- )
;

\ ---[ Writing data to file ]---------------------------------------------------

: underline    ( n -- )
   ugly
   if
      drop
   else
      s" \ " writeText
      2 - dup 0>
      if
         0
         do s" -" writeText loop
      else
         drop
      then
      write-nl
   then
;

: save-#line  ( n -- )
   ugly
   if
      drop
   else
      s" (" write-string
      n>string 3 write-string.R
      s" )" write-string
   then
;

: save-item  ( n1 n2 -- )  \ n1 is item, n2 is field width
   dup >r
   ugly if s" ~" else s"   ~" then
   ensure-space write-string n>string r> write-string.R 
;

: saveBaseDefTokData
   s" 0 value BaseDefaultData" write-stringline
   s" here to BaseDefaultData" write-stringline write-newline
   s" \ State     Base   Default  Token" dup >r ?write-stringline
   r@ underline
   #states @ 0
   ?do
      i save-#line
      i lexDefBase lex_base @ 4 save-item
      i lexDefBase lex_default @ 3 save-item
      i lex_tokens @ 3 save-item
      write-newline
   loop
   r> underline
;

: saveCheck&NextData
   ugly if decimal s" decimal" write-stringline then
   NextCheck nc_size @ write-number s" value maxCheck" write-string
   ugly if 36 base ! s" 36 base !" write-stringline then
   write-newline
   s" 0 value CheckNextData" write-stringline
   s" here to CheckNextData" write-stringline write-newline
   s" \ Index    Check    Next" dup >r ?write-stringline
   r@ underline
   NextCheck nc_size @ 0
   ?do
      i save-#line
      i lexNextCheck lex_check @ 3 save-item
      i lexNextCheck lex_next  @ 3 save-item
      write-newline
   loop
   r> underline
;

: save#states
  #states @ write-number
  s" value #states" write-string
  write-newline
;

: copy-string  ( caddr u -- caddr2 u2 )
   here swap dup chars allot 2dup 2>r cmove
   2r>
; 

\ The default output file name, may be redefined by use of setOutputFile 

2variable outputFile
: setOutputFile  ( caddr u -- )  copy-string outputFile 2! ;
s" lexstt.fth" setOutputFile

\ The default scanner filename, may be redefined by use of setScannerFile 

2variable scannerFile
: setScannerFile  ( caddr u -- ) copy-string scannerFile 2! ;
s" scanner.fth" setScannerFile

variable incl-scanner -1 incl-scanner !
: no-scanner  0 incl-scanner ! ;

: saveAllTables
   base @ >r decimal
   outputFile 2@ createFile
   s" \ State transition tables for a lexical scanner " writeLine
   s" \ Generated by LexGen " writeText version writeLine
   s" \ See http://www.qlikz.org/forth/lexgen/lexgen.html" writeLine
   write-nl
   s" base @ decimal" write-stringline
   s" : ~ 0 0 parse-name >number dup >r dup 1 min" write-stringline
   s" /string >number 2drop drop r> if negate then , ;" write-stringline
   write-newline
   save#states
   ugly if 36 base ! s" 36 base !" write-stringline then
   saveBaseDefTokData
   write-newline
   saveCheck&NextData
   s" base !" write-stringline write-nl
   incl-scanner @ if scannerFile 2@ append-file then
   closeFile
   r> base !
;

[defined] [-dev-] [if]

: writeChar  ( ch -- )
   pad c! pad 1 writeText
;

: saveChar  ( ch -- )
   dup bl >
   if
      s"    char " writeText writeChar
   else
      9 writeNumber.R
   then
;

: p
   cr .s key dup [char] q = swap [char] Q = or
   if ." Quit executed" cr quit then
;

: saveTT  ( caddr u -- )
   decimal
   createFile
   s" \ State transition table" writeLine writeNewLine
   #states @   writeNumber s"  set-states"  writeLine
   #symbols 1- writeNumber s"  set-symbols" writeLine writeNewLine
   #states @ 0
   do
      0        \ Next state count
      s" lex-state " writeText i 0 writeNumber.R 4 writeSpaces
      s" default " writeText i lexDefBase lex_default @ 0 writeNumber.R
      4 writeSpaces
      s" token " writeText i lex_tokens @ 0 writeNumber.R
      writeNewLine
      #symbols 0
      do
         i j getNextState dup ?validState
         if
            i saveChar s"    => " writeText writeNumber writeNewLine
            1+       \ Increment the next state count
         else
            drop
         then 
      loop
      s" #next-states" writeText 4 writeNumber.R writeNewLine
      s" \ -------------------------------------" writeLine
   loop
   closeFile
;
[then]

\ ------------------------------------------------------------------------------
[defined] [-dev-] [if] .( SaveTables.fth loaded. ) .s [then]
\ ------------------------------------------------------------------------------
