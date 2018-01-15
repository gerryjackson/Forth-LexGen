\ The test program for the source scanner
\ The state transition table has been tested by the file scanner test so that
\ does not need further testing, hence the small number of tests below

decimal

: get-token  ( -- caddr u tok )     \ Skips white space
   begin
      next-token dup "ws" =
   while
      drop 2drop
   repeat
;

[defined] [-dev-] [if]
   variable wait 0 wait !

   : ?wait  ( caddr u f -- caddr u f )
      wait @
      if
         >r cr 2dup type space r> p
      then
   ;
[else]
   : ?wait ;
[then]

: b"  ( --- caddr u )
   here [char] " parse
   string, count
;

: tmt  ( caun ... cau1 n -- tokn ... tok1 f )
   ." ."
   0 swap dup >r           ( -- caun ... cau1 f1 n )  ( R: -- n )
   begin dup
   while
      1- >r -rot           ( -- caun ... f1 cau1 )  ( R: -- n n' )
      get-token            ( -- caun ... f1 cau1 cau tok1 )
      2r> rot >r 2>r       ( -- caun ... f1 cau1 cau )  ( R: -- tok1 n n' )
      compare or r>        ( -- caun ... cau2 f' n' ) 
   repeat drop
   r>                      ( -- f n )  ( R: -- tok1 ... tokn )
   begin dup while 1- r> swap repeat   ( -- f tokn ... tok1 0 )
   drop
;

0 0 open-source
cr
t{ b" void" b" auto" b" static" b" extern" b" register" 5 tmt
   register  extern        static
auto          void        ->
   0 "void"   "auto"   "static"   "extern"   "register" }t
   
t{ b" the" b" then" b" there" b" thence" b" thencer" 5 tmt
   thencer thence there then the
   -> 0 "the" "then" "id" "thence" "id" }t

t{ b" \" 1 tmt \ -> 0 0 }t    \ Not recognised

close-source

cr .( LexGen source scanner tested.) cr cr

\ ---[ End of test program ]----------------------------------------------------

