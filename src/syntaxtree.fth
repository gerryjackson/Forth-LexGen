\ LexGen - Syntax Tree module - builds a syntax tree for a regular expression
\ Follows Algorithm 3.6 from p 142 of the Red Dragon book
\ "Compilers Principles, Techniques and Tools" Aho, Sethi and Ullman

\ Copyright (C) Gerry Jackson 2006, 2008, 2010, 2011, 2018

\ This software is covered by the MIT software license, a copy of which should
\ have accompanied this file. If not see https://opensource.org/licenses/MIT

[defined] [-dev-] [if] .( Loading SyntaxTree.fth ...) cr [then]

\ -----------------------------------------------------------------------------

1 cells constant cell

\ PositionSet will hold a pointer to the PositionSet class which will be
\ created when the final number of positions is known after completion of the
\ syntax tree

0 value PositionSet

\ -----------------------------------------------------------------------------
\ Array of pointers to leaf nodes to map from Position to its leaf node
\ We don't know the size of this array until the complete syntax tree has been
\ constructed.

0 value leaves
0 value maxPosition

: createLeaves    ( u -- )
   here to leaves
   dup  to maxPosition
   1+ cells allot
   leaves maxPosition 1+ cells erase
;

: ?position ( pos -- )
   maxPosition u>
   abort" Position out of range"
;

: ?node     ( node -- )
   0= abort" Invalid node in array leaves"
;

: getLeaf   ( pos -- node )
   dup ?position
   cells leaves + @
   dup ?node
;

: setLeaf   ( node pos -- )
   dup ?position
   cells leaves + !
;

\ ---[ Base class for syntax tree nodes ]---------------------------------------

object class
   cell var nullable       \ Boolean
   cell var firstPos       \ Pointer to a set
   cell var lastPos        \ Pointer to a set
   method calcNullable
   method calcFirstPos
   method calcLastPos
   method calcFollowPos
   method applyToTree      \ Applies a function to each tree node, depth first
   method displayNode      \ Displays node contents (for debugging)
   method loadLeafMap      \ In the base class so that applyToTree can be used
   method (clone)
end-class Node

\ Method applyToTree ( xt node ) executes xt at the children of node and then
\ at node itself. If node is a binary node the left subtree is processed first,
\ then the right subtree. Hence calling applyToTree of the syntax tree's root
\ node carries out a depth first traversal of the whole syntax tree. Typically
\ xt will be for the calc methods. Usage is e.g.
\      ' displayNode rootnode applyToTree
\ will display all node contents in the syntax tree
\ When other words e.g. (clone), leave items on the stack, care must be
\ taken to leave ( xt node) on top of the stack.

\ Of course applyToTree could push ( xt node ) onto the return stack before
\ executing the xt. But this has been avoided because syntax trees can get very
\ deep and, with applyToTree working recursively, there is a danger of return
\ stack overflow, using the return stack for (xt node) would triple return
\ stack usage (3 cells needed per recursion as opposed to 1). The method used
\ shares the load between the data and return stacks

:noname     ( class -- node )
   [ object :: new ]             ( -- node )
   false over nullable !
   0 over firstPos !
   0 over lastPos !
; Node defines new

\ calcNullable has no default implementation
\ calcFirstPos has no default implementation
\ calcLastPos has no default implementation

:noname     ( node -- )
   drop                       \ noop for most nodes
; Node defines calcFollowPos

\ applyToTree has no default implementation

:noname     ( caddr u node -- )
   -rot 7 over - 0 max >r
   cr
   cr type ."  node:" r> spaces dup .
   cr ." Nullable:    " dup nullable @ if ." true" else ." false" then
   cr ." FirstPos:    " dup firstPos @ show-set
   cr ." LastPos:     " lastPos @ show-set
; Node defines displayNode

:noname     ( node -- )    \ For all but leaf nodes
   drop
; Node defines loadLeafMap

\ The (clone) method is provided so that a regular expression can be given a
\ name which is then used in other regular expressions. When this happens the
\ entire syntax tree for the named regular expression has to be replicated.
\ Each node is responsible for replicating itself. Any such replication will
\ happen before firstPos, lastPos etc are calculated and so these will not be
\ copied. (clone) should not be called directly, but via applyToTree which
\ explains the stack signature of (clone) (see comments above). Although
\ (clone) does not use ( xt node ) it must leave them on top of the stack.

\ (clone) has no default implementation

\ ---[ Leaf nodes ]-------------------------------------------------------------
\
\ There are four types of leaf node:
\  1. holding a single character (or symbol), this avoids using a set
\     for this common case
\  2. an accepting node holding a return token, a sub class of 1.
\  3. holding a pointer to a character set
\  4. an empty leaf node (not currently used)
\ LeafNode is the base class for these classes

variable lastPosition 0 lastPosition !

: nextPosition
   lastPosition @ 1+ dup lastPosition !
;

Node class
   cell var position
   cell var followPos      \ Pointer to a set of following positions
   method ?LeafChar        ( ch node -- f )
   method ?acceptor        ( node -- false | token true )
end-class LeafNode

:noname     ( class -- node )
   [ Node :: new ]               ( -- node )
   nextPosition over position !
   0 over followPos !
; LeafNode defines new

:noname     ( node -- )
   dup position @ 0=             ( -- node flag )
   swap nullable !               ( -- )
; LeafNode defines calcNullable

: newLeafSet   ( node -- set )
   PositionSet new               ( -- node set )
   swap position @               ( -- set i )
   over add-member               ( -- set )
;

:noname     ( node -- )
   dup >r newLeafSet             ( -- set )
   r> firstPos !                 ( -- )
; LeafNode defines calcFirstPos

:noname
   dup >r newLeafSet
   r> lastPos !
; LeafNode defines calcLastPos

\ calcFollowPos is inherited (noop)

:noname     ( xt node -- )
   swap execute
; LeafNode defines applyToTree

:noname     ( node -- )
   >r s" Leaf" r@ [ Node :: displayNode ]
   cr ." Position:    " r@ position @ .
   cr ." FollowPos:   " r> followPos @ show-set
; LeafNode defines displayNode

:noname     ( node -- )
   dup position @ dup
   if setLeaf else 2drop then
; LeafNode defines loadLeafMap

\ Returns true if the leaf node is an acceptor node, returns false by default

:noname     ( node -- false )
   0=          ( -- false )   \ node <> 0 to run this definition
; LeafNode defines ?acceptor

\ ---[ Character leaf node ]----------------------------------------------------

LeafNode class
   cell var leafChar       \ Character owned by the leaf node
end-class LeafCharNode

:noname  ( ch class -- node )
   [ LeafNode :: new ]           ( -- ch node )
   tuck leafChar !               ( -- node )
; LeafCharNode defines new

:noname  ( ch node -- f )
   leafChar @ =
; LeafCharNode defines ?LeafChar

:noname     ( node -- )
   dup [ LeafNode :: displayNode ]
   cr ." Character:   " leafChar @ .
; LeafCharNode defines displayNode

:noname     ( xt node node1 -- node2 xt node )
   leafChar @ LeafCharNode new -rot    ( -- node2 xt node )
; LeafCharNode defines (clone)

\ ---[ Acceptor leaf node ]-----------------------------------------------------

\ Indicates that a pattern has been recognised

LeafCharNode class
   cell var token        \ Token to be returned when a pattern is recognised 
end-class AcceptorLeafNode

:noname     ( tok ch class -- node )  \ tok = token
   [ LeafCharNode :: new ]       ( -- tok node )
   tuck token !                  ( -- node )
; AcceptorLeafNode defines new

:noname     ( node -- tok true )
   token @ true
; AcceptorLeafNode defines ?acceptor

:noname     ( node -- )
   dup [ LeafCharNode :: displayNode ]
   cr ." Token:       " token @ .
; AcceptorLeafNode defines displayNode

\ Should never need to clone an AcceptorLeafNode but may as well include it
\ Or should it abort?

:noname     ( xt node node1 -- node2 xt node )
   dup token @ swap leafChar @   ( -- xt node tok ch )
   AcceptorLeafNode new -rot     ( -- node2 xt node )
; AcceptorLeafNode defines (clone)

\ ---[ Character class leaf node ]----------------------------------------------

LeafNode class
   cell var leafCharSet    \ Pointer to the character set owned by the node
end-class LeafCharSetNode

:noname  ( set class -- node )
   [ LeafNode :: new ]           ( -- set node )
   tuck leafCharSet !            ( -- node )
; LeafCharSetNode defines new

:noname  ( ch node -- f )
   leafCharSet @ swap member?
; LeafCharSetNode defines ?LeafChar

:noname     ( node -- )
   dup [ LeafNode :: displayNode ]
   cr ." Char Set:    " leafCharSet @ show-set
; LeafCharSetNode defines displayNode

:noname     ( xt node node1 -- node2 xt node )
   leafCharSet @ LeafCharSetNode new -rot    ( -- node2 xt node )
; LeafCharSetNode defines (clone)

\ ---[ Empty leaf node ]--------------------------------------------------------
0 [if]

\ Empty leaf nodes heven't been needed yet, therefore omitted for now
\ The <?> operator removes the need for it e.g. (a | empty) equals a?
\ For empty leaf nodes. One of these is needed for every empty option as they
\ may have followpos sets.
\ *** Beware - this code has never been tested

LeafNode class
   1 cells class-var emptypos    \ Points to a common empty position set
end-class EmptyLeafNode

0 EmptyLeafNode emptypos !

\ new is inherited

:noname     ( node -- )
   true swap nullable !          ( -- )
; EmptyLeafNode defines calcNullable

: getEmptyPos  ( node -- set )
   @ dup emptypos @ ?dup 0=      ( -- class true | class set false )
   if
      PositionSet new            ( -- class set ) \ An empty set
      dup rot emptypos ! dup     ( -- set set )
   then
   nip                           ( -- set )
;
   
:noname     ( node -- )
   dup getEmptyPos               ( -- node set )
   swap firstPos !               ( -- )
; EmptyLeafNode defines calcFirstPos

:noname
   dup getEmptyPos               ( -- node set )
   swap lastPos !                ( -- )
; EmptyLeafNode defines calcLastPos

\ calcFollowPos is inherited (noop)
\ applyToTree is inherited

:noname     ( node -- )
   [ LeafNode :: displayNode ]
   cr ." Empty leaf node"
; EmptyLeafNode defines displayNode

\ loadLeafMap is inherited

:noname  ( ch node -- f )
   2drop false
; EmptyLeafNode defines ?LeafChar

:noname     ( xt node node1 -- node2 xt node )
   drop EmptyLeafNode new -rot      ( -- node2 xt node )
; EmptyLeafNode defines (clone)

[then]

\ ---[ Base class for binary operators ]----------------------------------------

Node class
   cell var leftOperand             \ Pointers to nodes in the syntax tree
   cell var rightoperand
end-class BinaryNode

:noname     ( lop rop class -- node )
   [ Node :: new ]                  ( -- lop rop node )
   >r
   r@ rightOperand !
   r@ leftOperand  !
   r>                               ( -- node )
; BinaryNode defines new

:noname     ( node -- f1 f2 )
   dup  leftOperand  @ nullable @
   swap rightOperand @ nullable @
; BinaryNode defines calcNullable

\ calcFirstPos has no default implementation
\ calcLastPos has no default implementation
\ calcFollowPos is inherited (noop)

:noname     ( xt node -- )
   2dup leftOperand  @ applyToTree
   2dup rightOperand @ applyToTree
   swap execute
; BinaryNode defines applyToTree

:noname     ( caddr u node -- )
   dup >r [ Node :: displayNode ]
   cr ." Left child:  " r@ leftOperand @ .
   cr ." Right child: " r> rightOperand @ .
; BinaryNode defines displayNode

\ (clone) has no default implementation

\ ---[ Concatenation node ]-----------------------------------------------------

BinaryNode class
end-class CatNode

\ new is inherited

:noname     ( node -- )
   dup [ BinaryNode :: calcNullable ]  ( -- node f1 f2 )
   and swap nullable !
; CatNode defines calcNullable

:noname     ( node -- )
   >r
   r@ leftOperand @ dup firstPos @     ( -- node set1 )
   swap nullable @                     ( -- set1 f )
   if
      r@ rightOperand @ firstPos @     ( -- set1 set2 )
      PositionSet new                  ( -- set1 set2 set3 )
      tuck union tuck union            ( -- set3 )
   then
   r> firstPos !                       ( -- )
; CatNode defines calcFirstPos

:noname     ( node -- )
   >r
   r@ rightOperand @ dup lastPos @     ( -- node set2 )
   swap nullable @                     ( -- set2 f )
   if
      r@ leftOperand @ lastPos @       ( -- set2 set1 )
      PositionSet new                  ( -- set2 set1 set3 )
      tuck union tuck union            ( -- set3 )
   then
   r> lastPos !                        ( -- )
; CatNode defines calcLastPos

:noname     ( node -- )
   dup leftOperand @ lastPos @         ( -- node set1 )
   swap rightOperand @ firstPos @      ( -- set1 set2 )
   0 >r
   begin
      over r> 1+ get-next-member dup 0>  ( -- set1 set2 n flag )
   while
      dup >r getLeaf                   ( -- set1 set2 leaf )
      2dup followPos @ ?dup
      if
         union drop                    ( -- set1 set2 )
      else
         PositionSet new               ( -- set1 set2 leaf set2 set3 )
         tuck copy-set                 ( -- set1 set2 leaf set3 )
         swap followPos !              ( -- set1 set2 )
      then
   repeat
   drop 2drop
; CatNode defines calcFollowPos

\ applyToTree is inherited

:noname     ( node -- )
   s" Cat"
   rot [ BinaryNode :: displayNode ]
; CatNode defines displayNode

:noname     ( xt node left right node1 -- node2 xt node )
   drop CatNode new -rot                ( -- node2 xt node )
; CatNode defines (clone)

\ ---[ Alternation node for | operator ]----------------------------------------

BinaryNode class
end-class OrNode

\ new is inherited

:noname     ( node -- )
   dup [ BinaryNode :: calcNullable ]   ( -- node f1 f2 )
   or swap nullable !
; OrNode defines calcNullable

:noname     ( node -- )
   >r
   r@ leftOperand  @ firstPos @     ( -- set1 )
   r@ rightOperand @ firstPos @     ( -- set1 set2 )
   PositionSet new                  ( -- set1 set2 set3 )
   tuck union tuck union            ( -- set3 )
   r> firstPos !                    ( -- )
; OrNode defines calcFirstPos

:noname     ( node -- )
   >r
   r@ leftOperand  @ lastPos @      ( -- set1 )
   r@ rightOperand @ lastPos @      ( -- set1 set2 )
   PositionSet new                  ( -- set1 set2 set3 )
   tuck union tuck union            ( -- set3 )
   r> lastPos !                     ( -- )
; OrNode defines calcLastPos

\ calcFollowPos is inherited (noop)

\ applyToTree is inherited

:noname     ( node -- )
   s" Or"
   rot [ BinaryNode :: displayNode ]
; OrNode defines displayNode

:noname     ( xt node left right node1 -- node2 xt node )
   drop OrNode new -rot                ( -- node2 xt node )
; OrNode defines (clone)

\ ---[ Base class for unary operators * + ? ]-----------------------------------

Node class
   cell var operand
end-class UnaryNode

:noname     ( op class -- node )
   [ Node :: new ]                ( -- op node )
   >r
   r@ operand !
   r>
; UnaryNode defines new

:noname     ( node -- )
   true swap nullable !                      \ For * and ?
; UnaryNode defines calcNullable

:noname     ( node -- )          \ For * + and ?
   dup operand @ firstPos @               ( -- node set1 )
   PositionSet new tuck copy-set          ( -- node set2 )
   swap firstPos !
; UnaryNode defines calcFirstPos

:noname     ( node -- )          \ For * + and ?
   dup operand @ lastPos @                ( -- node set1 )
   PositionSet new tuck copy-set          ( -- node set2 )
   swap lastPos !
; UnaryNode defines calcLastPos

\ calcFollowPos is inherited (noop)

:noname     ( xt node -- )
   2dup operand @ applyToTree
   swap execute
; UnaryNode defines applyToTree

:noname     ( caddr u node -- )
   dup >r
   [ Node :: displayNode ]
   cr ." Child:       " r> leftOperand @ .
; UnaryNode defines displayNode

\ (clone) has no default implementation

\ ---[ For * operator ]---------------------------------------------------------

UnaryNode class
end-class StarNode

\ new is inherited
\ calcNullable is inherited
\ calcFirstPos is inherited
\ calcLastPos  is inherited

:noname     ( node -- )
   dup lastPos @ swap firstPos @       ( -- set1 set2 )
   0 >r
   begin
      over r> 1+ get-next-member       ( -- set1 set2 n )
      dup 0>
   while
      dup >r getLeaf                   ( -- set1 set2 leaf )
      2dup followPos @ ?dup
      if
         union drop                    ( -- set1 set2 )
      else
         PositionSet new               ( -- set1 set2 leaf set2 set3 )
         tuck copy-set                 ( -- set1 set2 leaf set3 )
         swap followPos !              ( -- set1 set2 )
      then
   repeat
   drop 2drop
; StarNode defines calcFollowPos

\ applyToTree is inherited

:noname     ( node -- )
   s" Star"
   rot [ UnaryNode :: displayNode ]
; StarNode defines displayNode

:noname     ( xt node op node1 -- node2 xt node )
   drop StarNode new -rot
; StarNode defines (clone)

\ ---[ For + operator ]---------------------------------------------------------

\ There is not a PlusNode as such.
\ As a+ is equivalent to aa*, the + operator <+> creates a special StarNode
\ followed by a CatNode as its parent. This class provides the special StarNode
\ which points to the operand node. However it does not calculate FirstPos and
\ LastPos, it simply copies their sets from the operand, these actions are
\ inherited from StarNode as is calcFollowPos. As it doesn't have its own
\ sub-tree, applyToTree does not recurse as the parent CatNode will have
\ already applied the operation to the sub-tree
\ Users should never use this class directly, only indirectly via <+>

StarNode class
end-class PlusStarNode

\ new is inherited
\ calcNullable  is inherited (sets nullable = true)
\ calcFirstPos  is inherited (copies operand's FirstPos)
\ calcLastPos   is inherited (copies operand's LastPos)
\ calcFollowPos is inherited

:noname     ( xt node -- )
   swap execute
; PlusStarNode defines applyToTree

:noname     ( node -- )
   s" PlusStar"
   rot [ UnaryNode :: displayNode ]
; PlusStarNode defines displayNode

\ (clone) for this type of node is a special case. Because this node is only
\ generated by the <+> operator it is guaranteed to be the right sub-tree
\ of its parent CatNode. Hence this version of (clone) will have been called
\ by the parent CatNode, hence the left sub-tree of that CatNode will be on
\ the stack and can be saved as the operand of this PlusStarNode. This
\ argument assumes that (clone) will be executed by calling applyToTree to
\ walk the syntax tree. Despite all this, implementation is simple.

:noname     ( op xt node node1 -- op node2 xt node )
   drop 2>r dup PlusStarNode new 2r>
; PlusStarNode defines (clone)

\ ---[ For ? operator ]---------------------------------------------------------

UnaryNode class
end-class OptNode

\ new is inherited
\ calcNullable is inherited
\ calcFirstPos is inherited
\ calcLastPos  is inherited
\ calcFollowPos is inherited (noop)
\ applyToTree is inherited

:noname     ( node -- )
   s" Opt"
   rot [ UnaryNode :: displayNode ]
; OptNode defines displayNode

:noname     ( xt node1 op node2 -- node3 xt node1 )
   drop OptNode new -rot
; OptNode defines (clone)

\ ---[ Operations on the syntax tree ]------------------------------------------

\ When the syntax tree is complete and the number of Positions (i.e. leaf
\ nodes) is known call createPositionSet to define the PositionSet class

: createPositionSet    ( "<spaces>name" -- )
   >in @ >r
   lastPosition @ :set
   r> >in !
   ' execute to PositionSet
;

\ Calculate nullable, firstPos and lastPos for each node
\ Must be called after createPositionSet

: updateSyntaxTree   ( node -- )    \ node is the root node
   ['] calcNullable  over applyToTree
   ['] calcFirstPos  over applyToTree
   ['] calcLastPos   over applyToTree
   drop
;

: createLeafMap   ( node -- )
   lastPosition @ createLeaves
   ['] loadLeafMap swap applyToTree
;

: updateFollowPos ( node -- )
   ['] calcFollowPos swap applyToTree
;

\ clone has to dup the xt and tree before calling applyToTree to get
\ the stack correct for when the root applyToTree calls (clone). This
\ has to be dropped on completion

: clone ( tree1 -- tree2 ) ['] (clone) swap 2dup applyToTree 2drop ;

\ ---[ Testing ]----------------------------------------------------------------

[defined] [test] [if]
\ Display the complete syntax tree

: showST ( tree -- ) ['] displaynode swap applyToTree ;
[then]

\ ------------------------------------------------------------------------------

[defined] [-dev-] [if] .( SyntaxTree.fth loaded. ) .s [then]

\ ------------------------------------------------------------------------------
