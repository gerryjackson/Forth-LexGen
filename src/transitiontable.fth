\ LexGen - Transition table module - Regular expression to DFA

[defined] [-dev-] [if] .( Loading TransitionTable.fth ...) cr [then]

\ Copyright (C) Gerry Jackson 2006, 2008, 2010, 2011, 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

\ ---[ Transition table  definitions ]------------------------------------------

\ Definition of transition table row sizes, these are values as they will be
\ set up by user calls

\ Transition table format

\             -------- Input symbols (characters) ----------
\ StateName      0         1         2       ....     max
\   State 0   state n0  state n1  state n2   ...    state nn
\   State 1                    etc
\   ...

\ State name is a pointer to a PositionSet object, size 1 cell
\ The columns contain a next state index number i.e. 0 for state 0 etc
\ The size of each next state element is 1 cell, this could be reduced to
\ 1 byte if the number of states < 256 if size of dataspace proves a problem
\ because transition tables can get big. However a future version may change
\ from using state number to using addresses to avoid conflicts with object
\ creation and to ease state minimisation when implemented (if ever!). This
\ would remove this possible reduction in the use of dataspace.

1 cells constant stateNameSize

0 value #symbols        \ Set by user call to setMaxChar
0 value rowSize         \ Ditto
0 value posSetSize      \ Set when number of leaf positions is known

: setRowSize   ( u -- )
   to #symbols
   stateNameSize #symbols cells + to rowSize
;

0 value TransTable    \ Address of start of transition table

\ Note that state>row adds on the size of dataspace that a PositionSet 
\ object uses to allow for those systems that allocate objects in dataspace.
\ This works as only one such object is created (deleted and re-used perhaps
\ many times) for each new state. If any system or changes lead to further
\ dataspace being used between new states then this also needs to be included
\ in the overall row size. Such cases will be detected by getNewState aborting

\ First we need to test if Mini-OOF objects are allocated in dataspace or not
\ and set the dataspaceObject flag if so. This flag is used by buildTransTable
\ to conditionally set posSetSize. Choose a convenient class such as
\ LeafCharNode to create an object, check if dataspace was used then delete it

here 0 LeafCharNode new                ( -- ad1 obj )
here rot - 0> value dataspaceObject    ( -- obj )
delete

: state>row    ( state -- ad )
   rowSize posSetSize + * TransTable +
\   rowSize * TransTable +
;

: symbol?   ( sym -- )
   #symbols u< 0= abort" get/setNextState: symbol out of range"
;

\ Given current state and symbol, get the next state from the table

: getNextState ( sym state1 -- state )
   over symbol?
   state>row stateNameSize + swap cells + @
;

\ Find the first next state that matches the given next state and return
\ its symbol value. If not found return -1.

: nextStateSymbol  ( state nextstate -- sym )
   #symbols 0
   do
      over i swap getNextState        ( -- state nextstate nextstate2 )
      over =
      if 2drop i unloop exit then
   loop
   2drop -1
; 

: ?validState ( state -- f )  \ True if state is valid
   0 >=
;

: setNextState ( state sym nextstate -- )
   -rot dup symbol?
   cells swap state>row stateNameSize + + !
;

: getStatePosSet  ( state -- set )
   state>row @
;

: setStatePosSet  ( set state -- )
   state>row !
;

-1 constant emptyNextState

variable #states      0 #states !

: getNewState       ( -- state )
   #states @                     ( -- state )
   dup state>row                 ( -- state ad )
   dup here u< abort" Transition table: unexpected data has been added"
   rowSize allot
   rowSize over + swap           ( -- state ad+u ad )    \ u cells
   do
      emptyNextState i ! cell
   +loop
   1 #states +!
;

\ ---[ Building the transition table ]------------------------------------------

\ The following words build the transition table. In practice when LexGen runs,
\ most processing time by far is spent executing this code. Execution time
\ of other LexGen components is negligible in comparison

\ Search the transition table to see if set already exists in a state
\ Returns (state true) if so

: searchTransTable ( set -- state true | x false )
   #states @ 0
   do
      i getStatePosSet           ( -- set set2 )
      over equal?                ( -- set f )
      if
         drop i true unloop exit ( -- state true )
      then
   loop
   false                         ( -- x false )
;

: isLeafChar   ( ch pos -- f )  \ f is true if ch is associated with leaf node
   getLeaf                             ( -- ch node )
   ?leafChar
;

\ Generates the Position set to be associated with the next state for a
\ character

: buildNextStateSet  ( set ch -- set2 )
   PositionSet new swap 2>r 0          ( -- set u )   ( R: -- set2 ch )
   begin
      2dup get-next-member             ( -- set u pos )
      dup 0 >=
   while
      r@ over isLeafChar               ( -- set u pos f )
      if
         dup getLeaf followPos @       ( -- set u pos set3 )
         dup 0<>
         if
            2r@ drop union             ( -- set u pos )
         else
            drop     \ An acceptor state
         then
      then
      nip 1+                           ( -- set pos+1 )
   repeat
   drop 2drop 2r> drop                 ( -- set2 )
;

: processPosSet   ( ch state -- )
   tuck getStatePosSet over            ( -- state ch set ch )
   buildNextStateSet                   ( -- state ch set2 )
   dup empty?
   if
      delete 2drop                     ( -- )
   else
      dup searchTransTable 0=          ( -- state ch set2 state f )
      if
         drop getNewState              ( -- state ch set2 state2 )
         tuck setStatePosSet           ( -- state ch state2 )  \ Save set2
         dup                           ( -- state ch state2 state2 )
      else
         over delete       \ set2 will not be used
      then
      nip setNextState                 ( -- )
   then
;

\ Generates next state entries etc for a single state

: processState    ( state -- )
   #symbols 0
   ?do
      i over processPosSet
   loop
   drop
;

: buildTransTable    ( node -- ) \ node is the root node of the syntax tree
   dataspaceObject
   if PositionSet cells/set @ 1+ cells else 0 then to posSetSize
   here to TransTable
   0 #states !
   getNewState                         ( -- node state )
   swap firstPos @ over setStatePosSet ( -- state )
   begin
      [char] . emit        \ Progess indicator
      dup #states @ <
   while
      dup processState
      1+                               ( -- state' )
   repeat
   drop
;

\ ------------------------------------------------------------------------------
\ Some words to assist debugging are in LexArrays.fth
\ ------------------------------------------------------------------------------

[defined] [-dev-] [if] .( TransitionTable.fth loaded. ) .s [then]

\ ------------------------------------------------------------------------------
