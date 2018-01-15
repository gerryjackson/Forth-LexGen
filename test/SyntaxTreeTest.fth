\ Tests for SyntaxTree.fth

\ Assumes enters with WFDev as the current directory

.( Loading SyntaxTreeTest.fth ...) cr

\ -----------------------------------------------------------------------------
\ Included files

s" test/tester.fr" included
decimal
\ s" ../library/mini_oof.fth" included
\ s" ../library/extended_mini_oof.fth" included
s" ../library/Sets.fth" included
s" ../library/sorting/shellsort.fth" included
s" ../Applications/LexGen" set-directory throw
s" ansify.fth" included
s" SyntaxTree.fth" included
s" TransitionTable.fth" included
s" LexArrays.fth" included

\ -----------------------------------------------------------------------------
\ Test helpers

\ Leaves the members of a set on the stack

: getMembers   ( set -- ix iy ... )
   ['] -rot apply-to-members
;

: getFirstPosSet  ( node -- i j ... ) firstPos  @ ?dup if getMembers endif ;
: getLastPosSet   ( node -- i j ... ) lastPos   @ ?dup if getMembers endif ;
: getFollowPosSet ( node -- i j ... ) followPos @ ?dup if getMembers endif ;

\ Character set will be 0 not used, a -> 1, b -> 2, c->3, ##->4
\ to make the transition table more manageable i.e. 5 columns

0 constant null  1 constant a  2 constant b  3 constant c  4 constant ##

: n>char ( n -- ch )
   case 0 of        0 endof
        1 of [char] a endof
        2 of [char] b endof
        3 of [char] c endof
        4 of [char] # endof
        abort" Invalid n for n>char"
   endcase
;

\ -----------------------------------------------------------------------------
\ The test will create the syntax tree in Fig 3.39 of Aho, Sethi & Ullman
\ testing the results nullable, firstPos, lastPos and followPos


cr .( Here: ) here . cr

Testing syntax tree creation

{{ a  LeafCharNode new constant leaf1 -> }}
{{ leaf1 position @ -> 1 }}
{{ leaf1 leafChar @ n>char -> char a }}
{{ b  LeafCharNode new constant leaf2 -> }}
{{ a  LeafCharNode new constant leaf3 -> }}
{{ b  LeafCharNode new constant leaf4 -> }}
{{ b  LeafCharNode new constant leaf5 -> }}
{{ ## LeafCharNode new constant leaf6 -> }}
{{ leaf6 position @ -> 6 }}

{{ leaf1 leaf2 OrNode new constant orn1 -> }}
{{ orn1 leftOperand  @ -> leaf1 }}
{{ orn1 rightOperand @ -> leaf2 }}

{{ orn1 StarNode new constant sn1 -> }}
{{ sn1 operand @ -> orn1 }}

{{ sn1 leaf3 CatNode new constant cn1 -> }}
{{ cn1 leftOperand  @ -> sn1   }}
{{ cn1 rightOperand @ -> leaf3 }}

{{ cn1 leaf4 CatNode new constant cn2 -> }}
{{ cn2 leaf5 CatNode new constant cn3 -> }}
{{ cn3 leaf6 CatNode new constant cn4 -> }}

Testing createFollowSet (now the no. of positions in the syntax tree is known)

{{ createPositionSet (PSC) -> }}  \ (PSC) is to the Position Set Class
{{ (PSC) -> PositionSet }}

Testing applyToTree updateSyntaxTree

variable ct 0 ct !
: countNodes ( node -- ) drop 1 ct +! ;

{{ ' countNodes cn4 applyToTree ct @ -> 12 }}

{{ cn4 upDateSyntaxTree -> }} \ calculate nullable, firstPos, lastPos for tree

{{ leaf1 nullable @ -> false }}
{{ leaf1 getFirstPosSet -> 1 }}
{{ leaf1 getLastPosSet  -> 1 }}

{{ leaf2 nullable @ -> false }}
{{ leaf2 getFirstPosSet -> 2 }}
{{ leaf2 getLastPosSet  -> 2 }}

{{ orn1 nullable @ -> false }}
{{ orn1 getFirstPosSet -> 1 2 }}
{{ orn1 getLastPosSet  -> 1 2 }}

{{ sn1 nullable @ -> true }}
{{ sn1 getFirstPosSet -> 1 2 }}
{{ sn1 getLastPosSet  -> 1 2 }}

{{ cn1 nullable @ -> false }}
{{ cn1 getFirstPosSet -> 1 2 3 }}
{{ cn1 getLastPosSet  -> 3 }}

{{ cn2 nullable @ -> false }}
{{ cn2 getFirstPosSet -> 1 2 3 }}
{{ cn2 getLastPosSet  -> 4 }}

{{ cn3 nullable @ -> false }}
{{ cn3 getFirstPosSet -> 1 2 3 }}
{{ cn3 getLastPosSet  -> 5 }}

{{ cn4 nullable @ -> false }}
{{ cn4 getFirstPosSet -> 1 2 3 }}
{{ cn4 getLastPosSet  -> 6 }}

Testing createLeafMap

{{ cn4 createLeafMap -> }}

{{ 1 getLeaf -> leaf1 }}
{{ 2 getLeaf -> leaf2 }}
{{ 3 getLeaf -> leaf3 }}
{{ 4 getLeaf -> leaf4 }}
{{ 5 getLeaf -> leaf5 }}
{{ 6 getLeaf -> leaf6 }}

Testing updateFollowPos

{{ cn4 updateFollowPos -> }}

{{ leaf1 getFollowPosSet -> 1 2 3 }}
{{ leaf2 getFollowPosSet -> 1 2 3 }}
{{ leaf3 getFollowPosSet -> 4 }}
{{ leaf4 getFollowPosSet -> 5 }}
{{ leaf5 getFollowPosSet -> 6 }}
{{ leaf6 getFollowPosSet ->   }}

\ -----------------------------------------------------------------------------
\ Transition table tests

5 setRowSize

{{ #symbols -> 5 }}
{{ rowSize -> 24 }}

here to TransTable

000 , 00  , 10 , 20 , 30 , 40 ,
111 , 01  , 11 , 21 , 31 , 41 ,
222 , 02  , 12 , 22 , 32 , 42 ,
333 , 03  , 13 , 23 , 33 , 43 ,
444 , 04  , 14 , 24 , 34 , 44 ,
555 , 05  , 15 , 25 , 35 , 45 ,
666 , 06  , 16 , 26 , 36 , 46 ,
777 , 07  , 17 , 27 , 37 , 47 ,
888 , 08  , 18 , 28 , 38 , 48 ,
999 , 09  , 19 , 29 , 39 , 49 ,

here TransTable - constant ttsize
10 #states !



{{ here #states @ state>row = -> true }}

{{ #states @ allotLexTokens -> }}   \ So that showTT can be used

{{ 4 3 getNextState ->  43 }}
{{ 4 9 getNextState ->  49 }}
{{ 0 1 getNextState ->   1 }}
{{ 0 0 getNextState ->   0 }}
{{ 0 getStatePosSet ->   0 }}
{{ 4 getStatePosSet -> 444 }}
{{ 9 getStatePosSet -> 999 }}

{{ 0 0 63 setNextState -> }}
{{ 0 0 getNextState -> 63 }}
{{ 1 0 getNextState -> 10 }}
{{ 0 getStatePosSet ->   0 }}

{{ 5 3 75 setNextState -> }}
{{ 2 5 getNextState -> 25 }}
{{ 3 5 getNextState -> 75 }}
{{ 4 5 getNextState -> 45 }}

{{ 9 4 87 setNextState -> }}
{{ 3 9 getNextState -> 39 }}
{{ 4 9 getNextState -> 87 }}

Testing transition table construction words

{{ leaf1 firstPos @ 0 setStatePosSet -> }}
{{ leaf2 firstPos @ 1 setStatePosSet -> }}
{{ orn1  firstPos @ 2 setStatePosSet -> }}
{{ sn1   firstPos @ 3 setStatePosSet -> }}
{{ leaf3 firstPos @ 4 setStatePosSet -> }}
{{ cn1   firstPos @ 5 setStatePosSet -> }}
{{ leaf4 firstPos @ 6 setStatePosSet -> }}
{{ cn2   firstPos @ 7 setStatePosSet -> }}
{{ cn3   firstPos @ 8 setStatePosSet -> }}
{{ cn4   lastPos  @ 9 setStatePosSet -> }}

{{ leaf5 firstPos @ searchTransTable nip -> false }}
{{ leaf1 firstPos @ searchTransTable -> 0 true }}
{{ cn1   firstPos @ searchTransTable -> 5 true }}
{{ cn4   lastPos  @ searchTransTable -> 9 true }}

{{ a 1 isLeafChar -> true  }}
{{ b 1 isLeafChar -> false }}
{{ b 5 isLeafChar -> true  }}
{{ a 5 isLeafChar -> false }}

{{ cn4 firstPos @ a buildNextStateSet getMembers -> 1 2 3 4 }}
{{ cn4 firstPos @ b buildNextStateSet getMembers -> 1 2 3   }}
{{ cn4 firstPos @ c buildNextStateSet getMembers ->         }}

Testing getNewState and processPosSet

\ First copy transition table to HERE so that new states can be appended

TransTable align here to TransTable
here ttsize dup allot move

{{ here getNewState swap here - negate #states @ -> 10 rowSize 11 }}
{{ 10 getStatePosSet -> emptyNextState }}
{{ 3 10 getNextState -> emptyNextState }}
{{ cn4 firstPos @ 10 setStatePosSet -> }}
{{ c 10 processPosSet -> }}      \ Should not add a new state to transition table
{{ #states @ -> 11 }}
{{ b 10 processPosSet -> }}      \ Should not add a new state to transition table
{{ #states @ -> 11 }}            \ as the state {1, 2, 3} already exists
{{ a 10 processPosSet -> }}      \ Should add new state {1, 2, 3, 4}
{{ #states @ -> 12 }}
{{ 11 getStatePosSet getMembers -> 1 2 3 4 }}   \ The new state
{{ null 10 getNextState ->  emptyNextState }}
{{ a    10 getNextState -> 11 }}
{{ b    10 getNextState ->  5 }}    \ state holding cn1::firstPos = {1, 2, 3}
{{ c    10 getNextState ->  emptyNextState }}
{{ ##   10 getNextState ->  emptyNextState }}


Testing buildTransTable    \ New table built for the Aho example

{{ cn4 buildTransTable -> }}
{{ #states @ -> 4 }}          \ Only four states generated
{{ 0 getStatePosSet getMembers -> 1 2 3   }}
{{ 1 getStatePosSet getMembers -> 1 2 3 4 }}
{{ 2 getStatePosSet getMembers -> 1 2 3 5 }}
{{ 3 getStatePosSet getMembers -> 1 2 3 6 }}

{{ null 0 getNextState -> emptyNextState }}
{{ a    0 getNextState -> 1 }}
{{ b    0 getNextState -> 0 }}
{{ c    0 getNextState -> emptyNextState }}
{{ ##   0 getNextState -> emptyNextState }}
{{ null 1 getNextState -> emptyNextState }}
{{ a    1 getNextState -> 1 }}
{{ b    1 getNextState -> 2 }}
{{ c    1 getNextState -> emptyNextState }}
{{ ##   1 getNextState -> emptyNextState }}
{{ null 2 getNextState -> emptyNextState }}
{{ a    2 getNextState -> 1 }}
{{ b    2 getNextState -> 3 }}
{{ c    2 getNextState -> emptyNextState }}
{{ ##   2 getNextState -> emptyNextState }}
{{ null 3 getNextState -> emptyNextState }}
{{ a    3 getNextState -> 1 }}
{{ b    3 getNextState -> 0 }}
{{ c    3 getNextState -> emptyNextState }}
{{ ##   3 getNextState -> emptyNextState }}

Testing construction of lexical scanner arrays

: getArrayData    ( xt offset -- )
   #states @ 0
   do
      i swap >r
      over execute r@ + @ swap
      r>
   loop
   2drop
;

{{ s" totalNextStates" StateArray new -> 0 totalNextStates -1 cells + }}
{{ s" nextStateCount"  StateArray new -> 0 nextStateCount  -1 cells + }}
{{ countNextStates -> }}
{{ ' totalNextStates 0 sa_count getArrayData -> 2 2 2 2 }}
{{ ' totalNextStates sortStateArray -> }}
{{ ' totalNextStates 0 sa_state getArrayData -> 0 1 2 3 }}
{{ ' totalNextStates 0 sa_count getArrayData -> 2 2 2 2 }}
{{ 0 countSymNextStates -> }}
{{ ' nextStateCount  0 sa_count getArrayData -> 0 1 0 0 }}
{{ 1 countSymNextStates -> }}
{{ ' nextStateCount  0 sa_count getArrayData -> 0 1 1 0 }}
{{ 2 countSymNextStates -> }}
{{ ' nextStateCount  0 sa_count getArrayData -> 0 1 0 1 }}
{{ 3 countSymNextStates -> }}
{{ ' nextStateCount  0 sa_count getArrayData -> 0 1 0 0 }}
{{ 2 countSymNextStates -> }}
{{ ' nextStateCount  0 sa_count getArrayData -> 0 1 0 1 }}
{{ ' nextStateCount sortStateArray -> }}
{{ ' nextStateCount 0 sa_state getArrayData -> 1 3 0 2 }}
{{ ' nextStateCount 0 sa_count getArrayData -> 1 1 0 0 }}

{{ s" lexDefBase" DefBase new -> }}
{{ ' lexDefBase 0 lex_default getArrayData -> 0 0 0 0 }}
{{ ' lexDefBase 0 lex_base    getArrayData -> 0 0 0 0 }}
quit {{ s" lexNextCheck" NextCheck new here -> 0 lexNextCheck  }}
quit
Testing getStateSymbol

{{ 2 0 nextStateCount sa_state @ 0 getStateSymbol ->  1 }}
{{ 2 0 nextStateCount sa_state @ 2 getStateSymbol -> -1 }}
{{ 2 1 nextStateCount sa_state @ 0 getStateSymbol ->  2 }}
{{ 2 1 nextStateCount sa_state @ 3 getStateSymbol -> -1 }}
{{ 2 1 nextStateCount sa_state @ #symbols 1- getStateSymbol -> -1 }}

Testing getDefault

{{ 2 countSymNextStates -> }}
{{ ' nextStateCount sortStateArray -> }}
{{ 2 getDefault -> 1 }}
{{ 1 countSymNextStates -> }}
{{ ' nextStateCount sortStateArray -> }}
{{ 1 getDefault -> -1 }}
{{ 0 countSymNextStates -> }}
{{ ' nextStateCount sortStateArray -> }}
{{ 0 getDefault -> 1 }}

Testing buildLexArrays

{{ countNextStates -> }}
{{ ' totalNextStates sortStateArray -> }}
{{ buildLexArrays -> }}

0 [if]
: showLexArrays
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


cr .( Here: ) here . cr

\ ' displayNode cn4 applyToTree cr cr

\ -----------------------------------------------------------------------------

.( SyntaxTreeTest.fth completed successfully. ) .s
