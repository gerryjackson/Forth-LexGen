\ Extended Mini-OOF is an extension to Mini-OOF written by Bernd Paysan

\ Extensions copyright (C) Gerry Jackson 2006

\ This software is free; you can redistribute it and/or modify it in
\ any way provided you acknowledge the original source and copyright

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ ---[ Versions ]---------------------------------------------------------------
\ 10/10/09 end-class factored to provide end-anonclass

\ ------------------------------------------------------------------------------
[defined] [-dev-] [if] .( Loading extended mini oof ...) cr [then]
\ ------------------------------------------------------------------------------
\ Mini-OOF definitions - by Bernd Paysan

: method  ( m v "<spaces>name" -- m' v )
   create
      over , swap cell+ swap
   does>
      @ over @ + @ execute
;

: var  ( m v size "<spaces>name" -- m v' )
   create over , +
   does> @ +
;

\ Original Mini-OOF definition of object deleted
\ create object 1 cells , 2 cells ,

: class  ( class -- class m v )
   dup 2@
;

: undefined_method
   true abort" Undefined method called"
;

\ end-class factored to provide end-anonclass for when an
\ unnamed class is required

: end-anonclass  ( class m v -- class2 )
   here >r , dup , 2 cells
   ?do
      ['] undefined_method ,
      1 cells
   +loop
   cell+ dup cell+
   r@ rot @ 2 cells /string move
   r>
;

: end-class  ( class m v "<spaces>name" -- )
   create end-anonclass drop
;

: defines  ( xt class "<spaces>name" -- )
   ' >body @ + !
;

\ Original Mini-OOF definition of new deleted
\ : new    ( class -- obj ) here over @ allot swap over ! ;

: ::
   ' >body @ + @ compile,
;

\ -----------------------------------------------------------------------------
\ Extensions to Mini-OOF

\ A class-var is equivalent to a var but accessed by class name rather than by
\ an object. Hence it is shared by all objects of that class.

: class-var  ( m v size "<spaces>name" -- m' v )
   aligned 2>r
   create dup ,
      r> + r>      ( -- m' v )
   does>           ( class -- addr )
      @ +
;

\ Class method - accessed via the class name, equivalent to
\ static methods

: class-method  ( m v "<spaces>name" -- m' v )
   create
      over , >r cell+ r>
   does>         ( class -- ? )
      @ over + @ execute
;

\ Definition of the parent object

2 cells 1 cells               ( -- m v )
   1 cells class-var pfree    \ Points to a free list of deleted objects
   class-method new
   method delete
create object , , 0 , 0 , 0 ,

\ The default definition of new either gets an object from the free list or,
\ if empty, allots it from dataspace

:noname  ( class -- obj )
   dup pfree @ ?dup            ( -- class obj obj | class 0 )
   if
      2dup @                   ( -- class obj class obj2 )
      swap pfree !             ( -- class obj )
   else
      align here over @ allot  ( -- class obj )
   then
   tuck !                      ( -- obj )
; object defines new

\ delete simply adds the object to the class's free list. The first
\ cell in an object is used as the next pointer in the list

:noname  ( obj -- )
   dup @                  ( -- obj class )
   2dup pfree @           ( -- obj class obj obj2 )
   swap !                 ( -- obj class )
   pfree !
; object defines delete

\ ::' ( class "<spaces>name" -- xt )
\ name is a method of class, xt is the execution token of code invoked by the
\ method. Purpose is to re-use code from one class in another when inheritance
\ is not possible. Use with caution!
\ Typical usage:  Class1 ::' method1 Class2 defines method2

: ::'  ( class "<spaces>name" -- xt )
   ' >body @        ( -- class offset )
   + @              ( -- xt )
;

\ -----------------------------------------------------------------------------
\ The interface provided by this file is:
\
\     constructor ( -- class )
\     invoke      ( ? obj -- ? )
\     construct   ( class "<spaces>name" -- ? )
\     getObject   ( "<spaces>name" -- obj )
\
\	stack contents after execution of construct and name
\	depend on user supplied definitions for new
\	and invoke
\
\ -----------------------------------------------------------------------------
\	A constructor class

\ constructor provides an alternative root class to that provided
\ by extended mini_oof i.e. object. It provides a means to have an
\ initialising constructor by redefinition of new and a means of
\ running user defined code when the constructed word is executed

object class
   method invoke       \ executed when the object is executed
end-class constructor

' drop constructor defines invoke

\ construct is a simple example of the Template Method pattern that
\ can be specialised by subclass redefinition of methods new and invoke

: construct  ( class "<spaces>name" -- )
   new              ( -- obj )
   create ,         ( -- )
   does>	@ invoke   ( -- ? )
;

\ getObject ( "<spaces>name" -- obj ) enables a user to obtain the
\ constructed object belonging to "name"

: getObject  ( "<spaces>name" -- obj )
   ' >body @
;

\ -----------------------------------------------------------------------------
[defined] [-dev-] [if] .( Extended mini oof loaded. ) .s [then]


