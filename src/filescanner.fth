\ ------------------------------------------------------------------------------
\ LexGen - filescanner.fth 
\ Optionally appended by LexGen to the state transition tables.
\ This scanner scans a separate text file as opposed to the Forth input source

\ Copyright (C) Gerry Jackson 2006, 2008, 2010, 2011, 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

\ ---[ Unique scanner id ]------------------------------------------------------

create lexgen-scanner-id-string  \ For use by other programs

\ ---[ Input text buffer ]------------------------------------------------------

base @ decimal
128 constant bufsize
base !
create inbuf bufsize 2 chars + allot   \ + space for CR LF

variable endOfLine
variable cursor
2variable currline

1 chars constant 1char

: init-cursor   ( caddr u -- )
   over cursor !
   + 1char - endOfLine !     \ endOfLine points to the last character
;

inbuf 0 init-cursor

: current-line  ( -- caddr u )  currline 2@ ;

: rest-of-line  ( -- caddr u )
   cursor @ endOfLine @ 1char + 
   over - 1char /
;

: set-cursor  ( caddr -- )  cursor ! ;

\ ---[ File handling ]----------------------------------------------------------

variable src-file  0 src-file !

: open-source  ( caddr u -- )
   r/o open-file abort" File opening error"
   src-file !
   inbuf 0 init-cursor
;

: close-source  ( -- )
   src-file @ close-file
   abort" File closing error"
   0 src-file !
;

: next-line  ( -- f )  \ f is false for end of file
   inbuf bufsize 2dup src-file @ read-line  ( -- caddr u1 u2 f ior )
   abort" Read from file failed"          ( -- caddr u1 u2 f )
   dup >r
   if
      tuck =                              ( -- caddr u2 f2 )
      abort" Input line is too long"
   else
      nip          \ End of file          ( -- caddr u2 )
   then
   2dup currline 2! init-cursor r>        ( -- f )
;

: get-char     ( -- ch )   \ ch = -1 is end of line
   cursor @ endOfLine @ over u<
   if
      drop -1              \ end of line reached
   else
      c@ 1char cursor +!   ( -- ch )
   then
;

-1 value eof-tok

: first-char  ( -- caddr ch | caddr eof )
   begin
      cursor @ get-char dup 0<   ( -- caddr ch 0 | caddr -1 -1 )
   while
      next-line                  ( -- caddr ch f )
   while
      2drop
   repeat
      drop eof-tok
   then
;

\ ---[ To access lex arrays ]---------------------------------------------------

1 cells constant 1cell
2 cells constant 2cells
3 cells constant 3cells

: BaseDefault  ( index -- ad )
   3cells * BaseDefaultData +
;

: CheckNext    ( index -- ad )
   2cells * CheckNextData +
;

\ These four definitions are for readability

: lexBase ; immediate         \ Compiles nothing
: lexDefault postpone cell+ ; immediate
: lexToken postpone cell+ postpone cell+ ; immediate 
: lexCheck ; immediate
: lexNext postpone cell+ ; immediate

\ ---[ Convert lex array indices to absolute addresses ]------------------------
\ Using absolute addresses is faster than using the array data to index the
\ arrays. An alternative is to replace the ~ in the LexTables data with a word
\ that does the conversion as the file is read

: ?valid  ( ad1 -- ad2 | 0 )   \ 0 if contents of ad1 negative
   @ dup 0<
   if
      drop 0
   else
      BaseDefault
   then
;

: >addresses
   BaseDefaultData #states 3cells * over + swap
   ?do
      i lexBase @ CheckNext i lexBase !
      i lexDefault ?valid i lexDefault !
      3cells
   +loop
   CheckNextData maxCheck 2cells * over + swap
   ?do
      i lexCheck ?valid i lexCheck !
      i lexNext  ?valid i lexNext !
      2cells
   +loop
   maxcheck CheckNext to maxCheck
;

>addresses    \ Do the conversion to absolute addresses

\ ------------------------------------------------------------------------------
\  Lexical scanning

\ next-state returns 0 if there is no valid next state

\ state and state2 are addresses, ch is a character converted to cells
\ by the caller

: next-state    ( state ch -- state2 )
   tuck over 2>r              ( -- ch' state )  ( R: -- ch' state )
   tuck lexBase @ +           ( -- state ad1 )
   dup CheckNextData maxCheck
   within                     ( -- state ad1 f ) \ f = 0 is out of range
   if
      dup lexCheck @ r@ =     ( -- state ad1 f )
      if
         nip lexNext @        ( -- state2 )
         2r> 2drop exit
      then
   then
   r> 2drop lexDefault @      ( -- state3 )  ( R: -- ch )
   r> over                    ( -- state3 ch f )
   if recurse exit then       ( -- state4 )
   drop                       ( -- 0 )
;

\ next-token returns  ( caddr u tok )
\    where tok = 0 for an unrecognised symbol
\              = eof-tok for end of file being scanned (preset to -1)
\              = otherwise a recognised symbol
\          (caddr u) is the symbol corresponding to the token

\ Note when a state has a valid token, this is remembered and we carry on
\ until we can get no further. This allows a longer lexeme to be recognised
\ and backtracking (within the line) if there is none such

: next-token    ( -- caddr u token )
   first-char dup eof-tok =            ( -- caddr1 ch f )
   if drop 0 eof-tok exit then         ( -- x 0 eof )
   2cells * >r dup char+               ( -- caddr1 caddr2 )
   0 BaseDefaultData r>                ( -- caddr1 caddr2 tok state ch' )
   begin
      next-state dup                   ( -- caddr1 caddr2 tok state2 state2)
   while
      dup lexToken @ 0>                ( -- caddr1 caddr2 tok state2 f )
      if
         >r 2drop cursor @             ( -- caddr1 caddr3 )
         r@ lexToken @ r>              ( -- caddr1 caddr3 tok2 state2 )
      then
      get-char 2cells *
   repeat
   drop >r 2dup cursor ! - r>          ( -- caddr1 u token )
;

\ ---[ Testing only ]-----------------------------------------------------------

: scan-file  ( caddr u -- )   \ (caddr u) is the file to be scanned
   open-source                ( -- )
   begin
      next-token dup eof-tok <>
   while
      dup >r cr 3 .r 3 spaces type
      r> 0= if 2 spaces ." invalid symbol" then
   repeat
   drop 2drop
   close-source
;

\ ------------------------------------------------------------------------------
