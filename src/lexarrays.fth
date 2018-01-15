\ LexGen - LexArrays module - converts the transition table into the data
\ structures to be used by the lexical scanner (See Aho, Sethi & Ullman, p145,
\ Fig 3.47)

\ Copyright (C) Gerry Jackson 2006, 2008, 2010, 2011, 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

[defined] [-dev-] [if] .( Loading LexArrays.fth...) cr [then]

\ ---[ Working arrays for transition table analysis ]---------------------------

\ variable #states in TransitionTable.fth holds the number of states which is
\ not known at compile time. The following class and words enable a state array
\ name to be defined now for compilation. The body is patched later e.g.
\ We define a state array name by:
\     createStateArray xyz
\ When the number of states is known we call
\     setStateArraySize    \ which updates the nvars field of StateArray
\ To create the array itself we do:
\     StateArray new xyz   \ which patches the object address into xyz

object class
   1  cells class-var unsized    \ <> 0 means size not yet set, used by new
   -1 cells var >array           \ Offset = 4 to start of array
   1  cells var sa_state         \ Offset = 0
   0  cells var sa_count         \ Offset = 4
end-class StateArray

-1 StateArray unsized !

: updateArrayName  ( caddr u obj -- )
   -rot get-current search-wordlist    ( -- obj 0 | obj xt f )
   0= abort" Array name not found"
   >body !                             ( -- )
;

:noname  ( caddr u class -- obj )
   dup unsized @
   if
      0 over unsized !
      #states @ 2* cells over +!
   then
   [ object :: new ]       ( -- caddr u obj )
   dup >r updateArrayName
   r>
; StateArray defines new

\ To define an array name for compilation

: createArray   ( -- )
   create
      0 ,                  \ Will be filled with an object address
   does>
      @ >array swap 2* cells +    ( i -- ad )
;

createArray totalNextStates   \ Number of next states from a state
createArray nextStateCount    \ Count of individual next states

\ ---[ Lex arrays ]-------------------------------------------------------------
\ The final arrays for the lexical scanner, names prefixed by lex
\ The size of the next and check array remains unknown until they have been
\ completed. Therefore they will be the last to be declared and will grow in
\ standard dataspace. Their class can be derived from StateArray for
\ convenience (to use createArray) but with new redefined to use dataspace.
\ As the default and base arrays are indexed by state they will be objects of
\ a class derived from StateArray.

StateArray class
   1 cells -                  \ Set var offset to 0
   1 cells var lex_default
   0 cells var lex_base       \ leaves initial object size at 1 cell
end-class DefBase

:noname  ( caddr u class -- )
   [ StateArray :: new ]         ( -- obj )
   >array                        ( -- ad1 )
   #states @ 2* cells erase
; DefBase defines new

createArray lexDefBase        \ Aho etc's default and base arrays

: getDefaultState  ( state -- defstate ) lexDefBase lex_default @ ;

StateArray class
   1 cells class-var nc_size \ Holds array size
   1 cells -
   1 cells var lex_next
   0 cells var lex_check
end-class NextCheck

:noname  ( caddr u class -- )
   0 over nc_size !           \ Will be incremented as more space is needed
   here swap ,                ( -- caddr u obj )
   updateArrayName            ( -- )
; NextCheck defines new

createArray lexNextCheck      \ Aho etc's next and check arrays

\ ---[ Sort the states ]--------------------------------------------------------

\ Count and sort the number of valid next states in each state

: countNextStates    ( -- )
   #states @ 0
   do
      0                       ( -- count )
      #symbols 0
      do
         i j getNextState ?validState  ( -- count f )
         if 1+ then          ( -- count+1 )
      loop
      i totalNextStates sa_count !
   loop
;

SortBase class
   1 cells class-var array_xt      \ Holds the xt of the array to be sorted
end-class StateSort

\ Compares on the #nextStates array but sorts the sortedNextStates and 
\ the #nextStates arrays (has to do both for method sortcompare)

:noname  ( i1 i2 class -- )
   array_xt @ tuck            ( -- i1 xt i2 xt )
   execute >r execute         ( -- ad1 )        ( R: -- ad2 )
   r@ 2@ rot dup 2@           ( -- d2 ad1 d1 )
   r> 2! 2!                   ( -- )            ( R: -- )
; StateSort defines sortswap

:noname  ( i1 i2 class -- f )
   array_xt @ tuck            ( -- i1 xt i2 xt )
   2>r execute sa_count @
   2r> execute sa_count @ >
; StateSort defines sortcompare

: sortStateArray  ( xt -- )
   dup StateSort array_xt !
   #states @ 0
   do                         \ Initialise the sa_state field in the array
      i over execute                ( -- xt ad )
      i swap sa_state !             ( -- xt )
   loop
   drop                             ( -- )
   StateSort #states @ ShellSort
;

: createArrays
   s" totalNextStates" StateArray new drop
   s" nextStateCount"  StateArray new drop
   s" lexDefBase" DefBase new
   s" lexNextCheck" NextCheck new
;

: resetNextStateCount
   #states @ 0
   do
      i i nextStateCount sa_state !
      0 i nextStateCount sa_count !
   loop
;

\ State 0 is the entry state and doesn't get counted

: countSymNextStates     ( state -- )
   resetNextStateCount
   #symbols 0
   do
      i over getNextState                    ( -- state state2 )
      dup ?validState over 0> and
      if
         1 swap nextStateCount sa_count +!   ( -- state )
         dup                                 ( -- state x )
      then
      drop
   loop
   drop
;

\ Get the symbol for the next valid next state, starting at sym1,
\ sym2 may be sym1, return -1 if no more available

: getNextSymbol    ( state sym1 -- sym2 )
   #symbols min 0 max            \ ensure sym1 is in range
   #symbols swap
   ?do                           ( -- state )
      i over getNextState        ( -- state state2 )
      dup ?validState
      if 2drop i unloop exit then
      drop                       ( -- state )
   loop
   drop -1
;

\ Search state for the next symbol >= sym1 whose next state is state2
\ Sym2 = the next symbol or -1 if not found

: getStateSymbol  ( state state2 sym1 -- sym2 )
   #symbols swap
   ?do                           ( -- state state1 )
      over i swap getNextState   ( -- state state1 state3 )
      over =                     ( -- state state1 f )
      if 2drop i unloop exit then      
   loop
   2drop -1
;

\ getDefault - see if the default entry can be used for state.
\ Returns the default state number if it can, -1 otherwise
\ For a next state d to be the default for state s three conditions must be
\ satisfied:
\ 1. every symbol that has next state d in state s must also have next state d
\    in state d, implemented as testDefault below
\ 2. Every symbol that has next state d in state d must have a non-zero next
\    state in state s, implemented as testState below
\ 3. Every symbol in s that has next state = -1 must also have next state = -1
\    in state d, not necessarily vice versa, testStateZeros

: testDefault  ( state defst -- f ) \ Returns true if defst can be the default
   0 >r
   begin
      2dup r> getStateSymbol              ( -- state defst sym )
      dup 0<
      if drop 2drop true exit then       ( -- true )
      dup 1+ >r over getNextState         ( -- state defst state2 )
      over <>
   until
   r> drop
   2drop false
;

: testState ( state defst -- f ) \ Returns true if defst can be the default
   #symbols 0
   do
      i over getNextState                 ( -- state defst state2 )
      over =
      if                                  ( -- state defst )
         over i swap getNextState
         ?validState 0=
         if 2drop false unloop exit then ( -- false )
      then
   loop
   2drop true
;

: testStateZeros ( state defst -- f ) \ Returns true if defst can be default
   #symbols 0
   do
      over i swap getNextState            ( -- state defst state2 )
      ?validState 0=
      if
         i over getNextState              ( -- state defst state3 )
         ?validState
         if 2drop false unloop exit then ( -- false )
      then
   loop
   2drop true
;

: getDefault  ( state -- s2 )
   #states @ 0
   do
      i nextStateCount dup sa_count @     ( -- state ad count )
      0= if drop leave then              \ Reached end of list
      sa_state @ 2dup <> over 0<> and     \ Cannot default to self or state 0
      if                                  ( -- state s2 )
         2dup 2dup 2dup testDefault       ( -- state s2 state s2 state s2 f )
         -rot testState and               ( -- state s2 state s2 f2 )
         -rot testStateZeros and          ( -- state s2 f3 )
         if nip unloop exit then
      then
      drop
   loop
   drop -1  
;

\ findBase for state such that there are free elements in the next/check arrays
\ for all symbols with a valid next state except for those symbols whose next
\ state is the default state2. Note state2 = -1 if there is no default.
\ The returned base is an index into the next/check arrays and may be negative
\ to ensure that the bottom elements of the arrays are utilised. But beware
\ that the fsm driver must then detect the symbol is within range before
\ accessing the arrays

: getNonDefaultSymbol      ( state sym defst -- sym2 )
   >r                                  ( -- state sym )
   begin
      over swap getNextSymbol          ( -- state sym2 )
      dup 0< 0=
   while
      2dup swap getNextState r@ =
   while
      1+                               ( -- state sym2+1 )
   repeat then
   r> drop nip                         ( -- sym2 )
;

\ If checkForSpace needs more space than already allocated in the next/check
\ arrays, the extra space is allocated and initialised, even if a later symbol
\ causes failure, this doesn't matter as it will be tried again with a bigger
\ base and the space allocated will be required anyway.
\ getMoreSpace assumes that nothing else allocates dataspace while the next 
\ and check arrays are being generated. Hence the -1 , -1 ,

: getMoreSpace ( sz -- )       \ sz is the new size >= old size
   1+ dup NextCheck nc_size dup @      ( -- sz' sz' ad oldsz )
   -rot !                              ( -- sz' oldsz )
   do             \ Shouldn't need ?do
      -1 , -1 ,
   loop
;

\ Note that base+sym >= 0 below

: checkForSpace   ( state sym defst base -- f) \ f = true if elements used
   2>r                                 ( -- state sym) ( R: -- defst base )
   begin
      dup 0< 0= dup                    ( -- state sym f1 f1 )
   while
      drop dup r@ +                    ( -- state sym b+s )
      dup NextCheck nc_size @ <        ( -- state sym b+s f2 )
      if
         lexNextCheck lex_next @ 0<    ( -- state sym f3 )
         dup 0= swap                   ( -- state sym f f3 )
      else     \ allocate more space and carry on
         getMoreSpace                  ( -- state sym )
         true dup                      ( -- state sym x true )
      then
   while                \ If f3 = 0 exit with f = true
      drop
      1+ over swap 2r@ drop            ( -- state state sym2 defst )
      getNonDefaultSymbol              ( -- state sym3 )
   repeat then
   -rot 2drop 2r> 2drop                ( -- f )
;

\ findBase could be made more efficient by locating the first free elements
\ in the next/check arrays and setting base = first_free - symbol
\ both at the start and within the loop. But as building lex arrays is much
\ much faster than building the transition table, why bother

: findBase  ( state defst -- base )    \ defst = default state
   >r dup 0 r@ getNonDefaultSymbol     ( -- state sym ) ( R: -- defst )
   dup 0<
   if
      0 >r                             \ All next states are default, base = 0
   else
      dup negate >r                    ( -- state sym )  ( R: -- defst base )
      begin
         2dup 2r@ checkForSpace        ( -- state sym f )
      while
         r> 1+ >r          \ Increment base and try again
      repeat
   then
   2drop 2r> nip                       ( -- base )
;

: setLexArrays    ( state defst base -- )
   >r swap r@ over                     ( -- defst state base state )
   lexDefBase lex_base !               ( -- defst state )
   2dup lexDefBase lex_default !
   0 rot >r                            ( -- state sym ) ( R: -- base defst )
   begin
      over swap r@ getNonDefaultSymbol ( -- state sym2 )
      dup 0< 0=
   while
      2dup swap getNextState           ( -- state sym2 ns )
      over 2r@ drop +                  ( -- state sym2 ns base+sym2 )
      lexNextCheck dup >r lex_next !   ( -- state sym2 )
      over r> lex_check !
      1+                               ( -- state sym2+1 )
   repeat
   2drop 2r> 2drop                     ( -- )
;

: buildLexArrays
   createArrays
   countNextStates
   ['] totalNextStates sortStateArray
   #states @ 0
   do
      i totalNextStates sa_state @        ( -- state )
      dup countSymNextStates
      ['] nextStateCount sortStateArray
      dup getDefault                      ( -- state state2 | state -1 )
      2dup findBase                       ( -- state state2 base )
      setLexArrays                        ( -- )
   loop
;

\ ---[ The tokens array ]-------------------------------------------------------

\ To load the fifth lex array which maps from acceptor state to the token
\ value to be returned to the caller

\ Cannot be sized until the number of states (from #states) is known
\ but we need to compile references to it hence the indirection to array data

: 1darray
   create 0 , 0 ,
   does>          ( i -- ad )
      2@ >r                   ( -- i n ) ( R: -- ad1 ) \ ad1 of array[0]
      over 0 rot within       ( -- i f )
      0= abort" Index to tokens array out of range"
      cells r> +              ( -- ad )
;

1darray lex_tokens

: getToken  ( state -- tok ) lex_tokens @ ;

\ allot and clear the lex_tokens array

: allotLexTokens   ( n -- )
   dup ['] lex_tokens >body   ( -- n n ad )
   align here over !
   cell+ ! 0                  ( -- n 0 )
   ?do
      0 ,
   loop
;

\ ## is the acceptor character, See Red Dragon book p 135. ## is appended to
\ all patterns that have to be recognised, as well as one at the end so that
\ acceptor states have a transition to make

0 value ##     \ The acceptor input symbol, set by user call to setMaxChar

\ getAcceptors, factor of loadLexTokens, stacks the number of acceptor
\ positions with their tokens in the FollowPos set. The lowest positions are
\ deepest on the stack i.e. posn is the lowest value pos

: getAcceptors ( set -- 2un ... 2u1 n )   \ 2un is a stack pair = tokn posn
   -1 2>r 0                            ( -- n ) ( R: -- set u )
   begin
      2r@ 1+ get-next-member           ( -- n pos )
      dup 0 >=
   while
      r> drop dup >r                   ( -- n pos ) ( R: -- set pos )
      getLeaf ?acceptor                ( -- n tok true | n false )
      if
         r@ rot 1+                     ( -- tok pos n' )
      then
   repeat
   drop 2r> 2drop
;

\ uniqueAcceptor? checks for a position being unique across all
\ states. Returns true if pos is unique.
\ Note that 2u1 = tok pos i.e. a pair.

: uniqueAcceptor? ( 2un...2u1 -- 2un...2u2 tok f )
   0 swap                        ( -- ... tok ct pos ) 
   #states @ 0
   ?do
      i getStatePosSet           ( -- ... tok ct pos set )
      over member?
      if swap 1+ swap then       ( -- ... tok ct' pos )
   loop
   drop
   dup 0= abort" ?uniqueAcceptor: count cannot be 0"
   1 =                           ( -- ... tok f )
;

\ saveLexToken is a factor of loadLexTokens which saves a token.
\ Note n > 0 and 2un = tokn posn
\ if n = 1 then save tok1 in lex_tokens
\ if n > 1 then save the token belonging to a unique acceptor
\ (an acceptor is unique if it only occurs once across all states).
\ More than one unique acceptor is an error (unsure about this so
\ assume this is true for now and see how it goes)
\ Following testing with C symbols this error did occur when the
\ symbol &= was accidentally entered twice. Therefore this error
\ probably indicates a symbol defined more than once.
\ 21/12 06 If this occurs issue a warning, the acceptor with the
\ lowest position will be used.
\ If none of the acceptors is unique, there is ambiguity in the
\ regular expressions, so use the one with the lowest position.
\ This corresponds with the earliest regular expression definition.
\ Note that getAcceptors places the position & token of the acceptor
\ with the lowest position deepest on the stack, so always saving the
\ token if the acceptor is unique automatically saves that of the
\ lowest position. Similarly if the acceptor is not unique.

: saveLexToken ( 2un...2u1 n state -- )
   over 1 =
   if          ( -- 2u1 1 state ) \ 2u1 = tok pos
      nip nip                    ( -- tok state )
      lex_tokens !               ( -- )
   else                 \ n > 1
      0 rot 0                    ( -- ... 2u1 state ct n 0 )
      ?do
         2>r uniqueAcceptor?     ( -- ... 2u1 tok1 f ) ( R: -- state ct )
         if       \ always save unique acceptor, leaves the lowest pos acceptor
            r> 1+ r@ swap >r     ( -- ... 2u2 tok1 state ) ( R: -- state ct' )
            lex_tokens !         ( -- ... 2u2 )
         else
            2r@ 0=               ( -- ... 2u2 tok1 state ct<>0 )
            if    \ No unique token yet saved, so use this one 
               lex_tokens !      ( -- ... 2u2 )
            else
               2drop             ( -- ... 2u2 )
            then
         then
         2r>                     ( -- ... 2u2 state ct )
      loop
      1 >
      if
         cr cr ." WARNING: one state has more than 1 token value"
         cr ." Probable cause: two identical symbol names"
         cr ." Token of the first declared symbol used" cr
      then
      drop                       ( -- )
   then
;

\ loadLexTokens must be called after buildLexArrays
\ Later possibly move a call to this into buildLexArrays itself

: loadLexTokens   ( -- )
   #states @ 0                   ( -- n 0 )
   over allotLexTokens
   ?do
      ## i getNextState          ( -- ns )  \ ns = next state
      ?validState
      if
         i getStatePosSet        ( -- set )
         getAcceptors            ( -- 2un...2u1 n )
         ?dup
         if i saveLexToken then  ( -- )
         i ## emptyNextState setNextState  \ Clear to prevent next/check entries
      then
   loop
;

\ ---[ Test and debug helpers ]-------------------------------------------------

\ For debugging, in this file to access lex_tokens
[defined] [-dev-] [if]
: showFP          \ Displays the FollowPos set for each state
   cr ." State         FollowPos" cr
   #states @ 0
   do
      i 4 .r
      4 spaces
      i getStatePosSet show-set
      cr
   loop
   cr
;

: showState  ( n -- )
   cr ." State:" dup 4 .r ." , Token:" dup lex_tokens @ 6 .r
   4 spaces
   #symbols 0
   do
      i 32 mod 0= if cr then
      i over getNextState dup ?validState
      if 3 .r else drop ."   -" then
   loop
   drop
;

: showAllStates  ( -- )
   #states @ 0
   do
      i showState
   loop
;

: showTT  ( -- )  \ Displays the complete Transition Table
   cr ." State    Token"
   5 spaces s" Next state"          ( -- caddr u )
   #symbols 3 * 3 + over - -rot     ( -- n caddr u )
   type spaces ." FollowPos" cr
   #states @ 0
   do
      i 4 .r
      4 spaces
      i lex_tokens @ 6 .r
      4 spaces
      #symbols 0
      do
         i j getNextState dup ?validState
         if 3 .r else drop ."   -" then
      loop
      4 spaces
      i getStatePosSet show-set
      cr
   loop
;

: showTokens   \ Displays the token associated with each state
   cr ." State   Token" cr
   #states @ 0
   ?do
      i 4 .r
      i lex_tokens @ 5 .r
      cr
   loop
;

: showLexArrays   \ Displays the final lex data arrays
   cr ." State Default Base" cr
   #states @ 0
   ?do
      i 4 .r
      i lexDefBase lex_default @ 7 .r
      i lexDefBase lex_base @ 6 .r
      cr
   loop
   cr ." Index   Next Check" cr
   NextCheck nc_size @ 0
   ?do
      i 4 .r
      i lexNextCheck lex_next @ 7 .r
      i lexNextCheck lex_check @ 6 .r
      cr
   loop
   cr
;
[then]

\ ------------------------------------------------------------------------------
[defined] [-dev-] [if] .( LexArrays.fth loaded. ) .s [then]
\ ------------------------------------------------------------------------------
