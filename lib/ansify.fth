\ Provides ANS compatibility for some common but non-standard words

s" [undefined]" pad c! pad char+ pad c@ move 
pad find nip 0=
[if]
   : [undefined]  ( "name" -- flag )
      bl word find nip 0=
   ; immediate
[then]

[undefined] [defined]
[if] : [defined] postpone [undefined] 0= ; immediate [then]

[undefined] -rot  [if] : -rot rot rot ; [then]

[undefined] <=    [if] : <= > 0= ; [then]

[undefined] >=    [if] : >= < 0= ; [then]

[undefined] endif [if] : endif postpone then ; immediate [then]

[undefined] on    [if] : on  ( ad -- )  -1 swap ! ; [then]

[undefined] off   [if] : off ( ad -- )   0 swap ! ; [then]

[undefined] parse-name
[if]   \ From Forth 200X web site
   : isspace? ( c -- f ) bl 1+ u< ;

   : isnotspace? ( c -- f ) isspace? 0= ;

   : xt-skip   ( addr1 n1 xt -- addr2 n2 ) \ gforth
      \ skip all characters satisfying xt ( c -- f )
      >r
      begin
         dup
      while
         over c@ r@ execute
      while
         1 /string
      repeat  then
      r> drop
   ;

   : parse-name ( "name" -- c-addr u )
      source >in @ /string
      ['] isspace? xt-skip over >r
      ['] isnotspace? xt-skip ( end-word restlen r: start-word )
      2dup 1 min + source drop - >in !
      drop r> tuck -
   ;
[then]

\ maxint and minint assume the use of 2's complement arithmetic

[undefined] maxint [if] 0 invert 1 rshift constant maxint [then]
[undefined] minint [if] maxint invert constant minint     [then]

[undefined] 1cell  [if] 1 cells constant 1cell  [then]
[undefined] 2cells [if] 2 cells constant 2cells [then]
[undefined] cell-  [if] : cell- 1cell - ;       [then]

[undefined] ]] [if]
\ Postponing a series of words e.g. : abc ... ]] (postponed words) [[ ... ;
\ Only handles words on 1 line, could improve to be multi-line

: ]]
   begin
      >in @ parse-name s" [[" compare
   while
      >in ! postpone postpone
   repeat
   drop
; immediate
[then]

\ PFE bug avoidance - fails with a negative offset to /string
s" hello!" 4 /string -4 /string nip 0=
[if]  \ Thanks to David Williams
   \ Assume that len >= 0 and (len -i) >= 0:
   : /string  ( addr len i -- addr+c[i] len-i )
      >r r@ - swap r> chars + swap
   ;
[then]

[undefined] defer [if]  \ From the Forth 200X web-site

: defer  ( "name" -- )
   create ['] abort ,
   does> ( ... -- ... )
   @ execute
;

: defer@  ( xt1 -- xt2 )  >body @ ;

: defer! ( xt2 xt1 -- )  >body ! ;

: is
   state @
   if
      postpone ['] postpone defer!
   else
      ' defer!
   then
; immediate

: action-of
   state @
   if
      postpone ['] postpone defer@
   else
      ' defer@
   then
; immediate
[then]

[undefined] 1char [if] 1 chars constant 1char [then]

[undefined] char- [if]
   1 chars 1 =
   [if]   : char-  ( caddr -- caddr')  postpone 1- ; immediate
   [else] : char-  ( caddr -- caddr')  1char - ;
   [then]
[then]

[undefined] place [if]
: place  ( caddr u ad -- )    \ As a counted string
   2dup 2>r char+ swap chars move 2r> c! ;
[then]

: string,  ( caddr u -- )
   here over 1+ chars allot place ;

[undefined] ," [if]
: ,"  [char] " parse string, ;
[then]

[undefined] >order [if]
: >order ( wid -- )
   >r get-order 1+ r> swap set-order
;
[then]

[undefined] execute-parsing [undefined] included-with-xt or [if]
wordlist constant execute-parsing-wordlist

get-current execute-parsing-wordlist set-current

\ X is prepended to the string, then the string is EVALUATEd
: X  ( xt -- )
   previous execute
   source >in ! drop       \ skip remaining input
; immediate         

set-current

: (exec-parsing)  ( ... xt u caddr -- ... )
   dup >r s" X " r> swap cmove
   tuck >r                          ( -- xt caddr u ) ( R: -- caddr )
   execute-parsing-wordlist >order
   ['] evaluate catch               ( -- )
   r> free throw throw
;

: execute-parsing ( ... caddr u xt -- ... )
   -rot dup >r 2 chars + tuck       ( -- xt u+2 caddr u+2 ) ( R: -- u )
   allocate throw                   ( -- xt u+2 caddr caddr2 )
   tuck 2 chars + r> cmove          ( -- xt u+2 caddr2 ) ( R: -- )
   (exec-parsing)                   ( -- )
;

\ included-with-xt has restrictions, do not use REFILL or \, beware of using
\ BL WORD or PARSE-NAME as some Forths, e.g. SwiftForth, do not recognise CR
\ or LF as white space.
\ When implemented properly could be named PROCESS-WITH

: included-with-xt  ( ... fid xt -- ... )
   swap dup file-size throw drop    ( -- xt fid u )
   tuck 2>r 2 chars +               ( -- xt u+2 )  ( R: -- fid u )
   dup allocate throw               ( -- xt u+2 caddr )
   dup 2 chars + r> r> read-file    ( -- xt u+2 caddr u2 ior ) ( R: -- )
   throw drop                       ( -- xt u+2 caddr )
   (exec-parsing)                   ( -- )
;
[then]

[undefined] char/ [if]
1 chars 1 = [if] : char/ ; immediate [else] : char/ 1 chars / ; [then]
[then]

[undefined] umax [if]
: umax  ( u1 u2 -- u1 | u2 )  2dup u> if drop else nip then ;
[then]

[undefined] umin [if]
: umin  ( u1 u2 -- u1 | u2 )  2dup u> if nip else drop then ;
[then]

[undefined] next-word [if]
: next-word  ( -- caddr u )  \ u=0 is end of file
   begin
      parse-name dup if exit then   ( -- caddr u )
      refill
   while
      2drop                         ( -- )
   repeat
;
[then]

\ ---[ End of file ]------------------------------------------------------------
