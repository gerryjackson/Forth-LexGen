\ Sets.fth  Adapted from code in Anton Ertl's gray.fs distributed with gforth

\ Copyright 1990, 1991, 1994, 2003 Martin Anton Ertl

\     This program is free software; you can redistribute it and/or modify
\     it under the terms of the GNU General Public License as published by
\     the Free Software Foundation; either version 2 of the License, or
\     (at your option) any later version.

\     This program is distributed in the hope that it will be useful,
\     but WITHOUT ANY WARRANTY; without even the implied warranty of
\     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\     GNU General Public License for more details.

\     You should have received a copy of the GNU General Public License
\     along with this program; if not, write to the Free Software
\     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.

\ ---[ Changes ]---------------------------------------------------------------

\ 17 August 2010 Added get-maxmember for updated LexGen
\ 2 April 2010 Renamed bits/cell to cell-bits to avoid clash with Grace
\              generated parsers

\ -----------------------------------------------------------------------------

\ Ideas for improvement
\  1. Have an alternative class for sets where the number of members is less
\     then cell-bits. This would allow more efficient operations on such sets.
\     The set operations would have to become methods
\  2. Change the stack diagram for union and intersection to:
\            ( set1 set2 -- set2 )
\     as experience has shown that this is usually required.

\ Changes:
\ 1. The use of Mini-OOF so that different sized sets may be used
\    simultaneously
\ 2. intersection and union always put the result into set2, they do not 
\    create a new set for the result
\ 3. copy-set copies the contents of one set to another, it does not create
\    a new set
\ 4. Additional operations: drop-member drop-members get-next-member
\                           ones is-member empty? equal?
\ 5. 11/5/08 Added operations get-set-size get-bit-vector
\ 6. 27/5/08 Added a variant of ones that counts the number of set bits in a
\    cell for non-32 bit cells (a general definition but slower)

\ Reservations about this code (and the original)
\ 1. No checking e.g. for bit number out of range or for sets being of the
\    same type in binary operations. Perhaps this could be included using
\    conditional compilation, in for development, out for operational use.
\    What to do if checks fail, abort, throw an exception or return an
\    error code?
\ 2. Need a reset operation to make set empty e.g. clear-set (see tests)
\ 3. binary-set-operation and hence intersection and union, operate on all
\    complete cells i.e. also on any unused bits in the last cell. Similarly
\    binary-set-test? and hence subset? and disjoint?

\ sets - represented as bit arrays
\ bits that represent no elements, must be 0
\ all operations assume valid parameters
\ elements must be unsigned numbers

[defined] [-dev-] [if] .( Loading Sets.fth ...) cr [then]

\ ------------------------------------------------------------------------------
\ Get or calculate number of bits in 1 cell

1 cells constant cell
s" address-unit-bits" environment? 0=
[if]
   : (cell-bits)  ( -- n )
      1 0 invert dup 1 rshift xor
      begin
         dup 1 = 0=
      while
         1 rshift swap 1+ swap
      repeat
      drop
   ;
   (cell-bits)
[else]
   cells
[then]

constant cell-bits

\ -----------------------------------------------------------------------------
\ For definition of sets

\ The SetBase class

\ The original colon definitions have not been converted into methods as they
\ are unlikely to ever need re-definition.
\ In the stack signatures: set is a Set object, bv is a bit vector

object class
   0 var bitvec
   1 cells class-var cells/set
   1 cells class-var maxMember
end-class SetBase

\ SetBase::new creates an empty set. It removes the need for the sequence:
\		empty copy-set
\ in the original implementation

:noname  ( class -- set )  \ class method new
   dup [ object :: new ]         ( -- class set )
   swap cells/set @ cells        ( -- set u )
   over bitvec tuck + swap       ( -- set ad2 ad1 )
   ?do                           ( -- set )
      0 i !                      \ Clear the new set
      1 cells
   +loop
; SetBase defines new

\ :set defines a new set class called <name> initialised for u members
\ Execution: <name>  ( -- class ) leaves its class address on the stack
\ Therefore <name> new creates an empty set object
\ It cheats by setting the nvars field of the set class to include the
\ size of the set i.e. it uses knowledge of Mini-OOF internals

: :set   ( u "<spaces>name" -- )
   SetBase class >in @ >r end-class r> >in !
   ' execute >r                     ( R: -- class ) \ of the new set
   dup r@ maxMember !
   cell-bits / 1+                   ( -- ncells )
   dup r@ cells/set !
   cells r> +!                      ( -- ) \ Update object size
;

\ -----------------------------------------------------------------------------
\ Internal operations

\ returns a cell with only bit #u set

: decode    ( u -- w )
   1 swap lshift
;

\ addr1*cell-bits+u1=addr2*cell-bits+u2, u2<cell-bits

: normalize-bit-addr ( addr1 u1 -- addr2 u2 )
   cell-bits /mod
   cells rot +
   swap
;

\ Updates set2 from set1 and set2 by applying xt to members
\ where:    xt execute      does ( w1 w2 -- w2' )
\ e.g. ' or binary-set-operation  is the union operation

: binary-set-operation  ( set1 set2 xt -- )
   over @                           ( -- set1 set2 xt class )
   2>r bitvec swap bitvec swap 2r>  ( -- bv1 bv2 xt class )
   cells/set @ 0
   ?do
      >r
      over @ over @ r@ execute      ( -- bv1 bv2 w2' )
      over !                        ( -- bv1 bv2 )
      cell+ swap cell+ swap         ( -- bv1' bv2' )
      r>
   loop
   drop 2drop
;

\ returns true, if xt binary-set-operation returns empty
\ e.g. set1 set2 ' and binary-set-test?  is true, if set1 and set2
\ are disjoint, i.e. they contain no common members

: binary-set-test?   ( set1 set2 xt -- f )
   over @ 2>r true -rot             ( -- true set1 set2 )
   bitvec swap bitvec swap 2r>      ( -- true bv1 bv2 xt class )
   cells/set @ 0
   ?do
      >r
      over @ over @ r@ execute
      if
         rot drop false -rot  \ Could be? r> 2drop 2drop false unloop exit
      then
      cell+ swap cell+ swap
      r>
   loop
   drop 2drop
;

: notb&and  ( w1 w2 -- w3 )
   invert and
;

\ Remove selected members from a set cell

: (drop-members)  ( w1 w2 -- w3 )
   swap notb&and
;

\ member? returns true if u is in set )

: member?   ( set u -- f )
   >r bitvec r>
   normalize-bit-addr
   decode
   swap @ and
   0<>
;

cell-bits 32 = [if]
\ There is an improvement to this algorithm, see snippets\countbits.txt

   base @ 8 base ! 

   : ones ( uint32 -- #bits-set )
      dup 1 and swap 1 rshift
      dup 1 rshift 33333333333 and 
      dup 1 rshift 33333333333 and 
      +  - 
      dup 3 rshift + 30707070707 and 
      77 mod +
   ;

   base !

[else]

   : ones  ( u -- #bits-set )
      0 swap
      begin
         dup
      while
         dup 1- and        \ clear the ls bit that is set
         swap 1+ swap
      repeat
      drop
   ;

[then]

\ -----------------------------------------------------------------------------
\ Interface

\ Copies the contents of set1 into set2

: copy-set  ( set1 set2 -- )
   dup @ cells/set @ cells >r
   bitvec swap bitvec swap r>     ( -- bv1 bv2 u )
   move
;

\ changes set to include bit u )

: add-member   ( u set -- )
   bitvec swap normalize-bit-addr
   decode
   over @ or swap !
;

\ drop-member removes member u from a set

: drop-member   ( u set -- )
   bitvec swap normalize-bit-addr   ( -- ad u2 )
   decode invert                    ( -- ad mask )
   over @ and swap !
;

\ Remove the members of set1 from set2

: drop-members  ( set1 set2 -- )
   ['] (drop-members) binary-set-operation
;

\ Returns true if u is a member of the set

: is-member  ( u set -- f )
   swap member?
;

\ executes [ u -- ] for every member of set

: apply-to-members   ( set [ u -- ] -- )
   over @                        ( -- set xt class )
   cells/set @ cell-bits * 0
   ?do
      over i member?
      if
         i over execute
      then
   loop
   2drop
;

\ set1 union set2 -> set2

: union    ( set1 set2 -- )
   ['] or binary-set-operation
;

\ set1 intersection set2 -> set2

: intersection    ( set1 set2 -- )
   ['] and binary-set-operation
;

\ returns true if every member of set1 is in set2

: subset?   ( set1 set2 -- f )
   ['] notb&and binary-set-test?
;

\ returns true if set1 and set2 have no common members

: disjoint? ( set1 set2 -- f )
   ['] and binary-set-test?
;

\ GWJ addition, returns true if set is empty

: empty?    ( set -- f )
   dup disjoint?
;

\ GWJ addition, returns true if set1 and set2 have exactly the same members

: equal?    ( set1 set2 -- f )
   ['] <> binary-set-test?
;

\ GWJ addition to iterate through set members.
\ Returns the next set member whose bit number >= u
\ or -1 if no more members

: get-next-member   ( set u -- n )
   over @                        ( -- set u class )
   cells/set @ cell-bits * swap
   ?do                           ( -- set )
      dup i member?
      if
         drop i unloop exit
      then
   loop
   drop -1
;

: count-members  ( set -- u )
   dup bitvec 0 rot @          ( -- bv u class )
   cells/set @ 0
   ?do
      over @ ones +            ( -- bv u' )
      swap cell+ swap          ( -- bv' u' )
   loop
   nip
;

: get-set-size  ( set -- n )  @ cells/set @ ;   \ Returns number of cells

\ : copy-bit-vector  ( ad set -- ) \ ad is 'to' address
\   dup bitvec -rot cells/set @ move
\ ;

: get-bit-vector  ( set -- vecn...vec1 n )  \ vec1 holds ls bits
   dup @ cells/set @ >r
   bitvec r@ 1- cells over +  ( -- ad1 ad1+4n-4 )
   do
      i @                     ( -- vecn )
      -1 cells
   +loop
   r>                         ( -- vecn...vec1 n )
;

: get-maxmember  ( set -- u )  @ maxMember @ ;

\ -----------------------------------------------------------------------------
\ For debugging

: show-set ( set -- )
   ?dup
   if
      ['] . apply-to-members
   else
      ." <not defined>"
   then
;

\ -----------------------------------------------------------------------------

[defined] [-dev-] [if] .( Sets.fth loaded. ) .s [then]

