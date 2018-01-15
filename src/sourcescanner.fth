\ ------------------------------------------------------------------------------
\ LexGen - sourcescanner.fth
\ Optionally appended by LexGen to the state transition tables.
\ This scanner scans the Forth input source as opposed to a general text file
\ and so uses >IN, SOURCE etc to access the text
 
\ Copyright (C) Gerry Jackson 2006, 2008, 2010, 2011, 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

\ ---[ Unique scanner id ]------------------------------------------------------

create lexgen-scanner-id-string  \ For use by other programs

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

\ ---[ Input source access ]----------------------------------------------------

1 chars constant 1char

: current-line  ( -- caddr u )  postpone source ; immediate
: rest-of-line  ( -- caddr u )  source >in @ /string ;
: get-cursor    ( -- caddr )    rest-of-line drop ;
: set-cursor    ( caddr -- )    source drop - 1char / >in ! ;
: next-line     ( -- f )        postpone refill ; immediate
: open-source   ( x x -- )  2drop  ;
: close-source  ( -- )          ;

-1 value eof-tok  \ A value allows the user to set it, don't assume it stays -1

: get-char  ( -- ch | -1 )
   source >in @ tuck <=       ( -- c-addr1 u2 f )
   if 2drop -1 exit then
   chars + c@                 ( -- ch )
   1 >in +!
;

\ ---[ Lexical scanning ]-------------------------------------------------------
\ The user should call next-token 

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
\              = eof-tok for end of the file being scanned (preset to -1)
\              = otherwise a recognised symbol
\          (caddr u) is the symbol corresponding to the token

\ Note when a state has a valid token, this is remembered and we carry on
\ until we can get no further. This allows a longer lexeme to be recognised
\ and backtracking (within the line) if there is none such

: end-of-source?  ( -- f )  \ Return true if no more source
   begin
      rest-of-line nip 1- 0< dup
   while
      drop refill 0=
   until
      -1
   then
;

: next-token    ( -- caddr u token )
   end-of-source?                   ( -- f )
   if pad 0 eof-tok exit then       \ End of file reached
   rest-of-line over + swap 2>r     ( R: -- end caddr )
   r@ dup char+ 0 BaseDefaultData   ( -- caddr caddr2 tok state )
   begin
      2r@ >
   while
      r> dup char+ >r
      c@ 2cells * next-state        ( -- caddr caddr2 tok state2 )
      dup
   while
      dup lexToken @ 0>
      if
         >r 2drop 2r@ lexToken @ r> ( -- caddr caddr3 tok2 state2)
      then
   repeat then
   2r> 2drop drop
   >r 2dup set-cursor - r>          ( -- caddr u tok )
;

\ ---[ Testing only ]-----------------------------------------------------------

: scan-file  ( -- )   \ scans the text following in the input source
   begin
      next-token dup eof-tok <>
   while
      dup >r cr 3 .r 3 spaces type
      r> 0= if 2 spaces ." invalid symbol" then
   repeat
   drop 2drop
;

\ ------------------------------------------------------------------------------
