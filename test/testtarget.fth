\ The test program for the generated state transition table and file scanner

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

: tt  ( caddr u -- f tok | eof )
   get-token dup >r eof-tok =    ( -- caddr u caddr2 u2 f1 )
   ?wait
   if
      2drop 2drop                ( -- )
   else
      ." ."
      compare                    ( -- f )
   then
   r>
;

s" test\test.txt" open-source
cr
t{ s" void" tt -> 0 "void" }t
t{ next-token -rot s"  " compare -> "ws" 0 }t
t{ s" auto" tt -> 0 "auto" }t
t{ next-token -rot s"     " compare -> "ws" 0 }t
t{ s" static" tt -> 0 "static" }t
t{ s" extern" tt -> 0 "extern" }t
t{ s" register" tt -> 0 "register" }t
t{ s" typedef" tt -> 0 "typedef" }t
t{ s" char" tt -> 0 "char" }t
t{ s" float" tt -> 0 "float" }t
t{ s" double" tt -> 0 "double" }t
t{ s" int" tt -> 0 "int" }t
t{ s" short" tt -> 0 "short" }t
t{ s" long" tt -> 0 "long" }t
t{ s" unsigned" tt -> 0 "unsigned" }t
t{ s" struct" tt -> 0 "struct" }t
t{ s" union" tt -> 0 "union" }t
t{ s" enum" tt -> 0 "enum" }t
t{ s" if" tt -> 0 "if" }t
t{ s" else" tt -> 0 "else" }t
t{ s" while" tt -> 0 "while" }t
t{ s" do" tt -> 0 "do" }t
t{ s" for" tt -> 0 "for" }t
t{ s" switch" tt -> 0 "switch" }t
t{ s" break" tt -> 0 "break" }t
t{ s" continue" tt -> 0 "continue" }t
t{ s" return" tt -> 0 "return" }t
t{ s" goto" tt -> 0 "goto" }t
t{ s" case" tt -> 0 "case" }t
t{ s" default" tt -> 0 "default" }t
t{ s" sizeof" tt -> 0 "sizeof" }t
t{ s" ;" tt -> 0 ";" }t
t{ s" {" tt -> 0 "{" }t
t{ s" }" tt -> 0 "}" }t
t{ s" ," tt -> 0 "," }t
t{ s" :" tt -> 0 ":" }t
t{ s" =" tt -> 0 "=" }t
t{ s" (" tt -> 0 "(" }t
t{ s" )" tt -> 0 ")" }t
t{ s" [" tt -> 0 "[" }t
t{ s" ]" tt -> 0 "]" }t
t{ s" *" tt -> 0 "*" }t
t{ s" ." tt -> 0 "." }t
t{ s" ->" tt -> 0 "->" }t
t{ s" ++" tt -> 0 "++" }t
t{ s" --" tt -> 0 "--" }t
t{ s" &" tt -> 0 "&" }t
t{ s" -" tt -> 0 "-" }t
t{ s" !" tt -> 0 "!" }t
t{ s" ~" tt -> 0 "~" }t
t{ s" /" tt -> 0 "/" }t
t{ s" %" tt -> 0 "%" }t
t{ s" +" tt -> 0 "+" }t
t{ s" <<" tt -> 0 "<<" }t
t{ s" >>" tt -> 0 ">>" }t
t{ s" <" tt -> 0 "<" }t
t{ s" <=" tt -> 0 "<=" }t
t{ s" >=" tt -> 0 ">=" }t
t{ s" >" tt -> 0 ">" }t
t{ s" ==" tt -> 0 "==" }t
t{ s" !=" tt -> 0 "!=" }t
t{ s" ^" tt -> 0 "^" }t
t{ s" |" tt -> 0 "|" }t
t{ s" &&" tt -> 0 "&&" }t
t{ s" ||" tt -> 0 "||" }t
t{ s" ?" tt -> 0 "?" }t
t{ s" +=" tt -> 0 "+=" }t
t{ s" -=" tt -> 0 "-=" }t
t{ s" *=" tt -> 0 "*=" }t
t{ s" /=" tt -> 0 "/=" }t
t{ s" %=" tt -> 0 "%=" }t
t{ s" >>=" tt -> 0 ">>=" }t
t{ s" <<=" tt -> 0 "<<=" }t
t{ s" &=" tt -> 0 "&=" }t
t{ s" ^=" tt -> 0 "^=" }t
t{ s" |=" tt -> 0 "|=" }t
t{ s" abc" tt -> 0 "id" }t
t{ s" stat" tt -> 0 "id" }t
t{ s" swit987" tt -> 0 "id" }t
t{ s" voida" tt -> 0 "id" }t
t{ s" void" tt -> 0 "void" }t
t{ s" +" tt -> 0 "+" }t
t{ s" ;" tt -> 0 ";" }t
t{ s" {" tt -> 0 "{" }t
t{ s" ," tt -> 0 "," }t
t{ s" :" tt -> 0 ":" }t
t{ s" ++" tt -> 0 "++" }t
t{ s" ++" tt -> 0 "++" }t
t{ s" +" tt -> 0 "+" }t
t{ s" <<=" tt -> 0 "<<=" }t
t{ s" <<" tt -> 0 "<<" }t
t{ s" <" tt -> 0 "<" }t
t{ s" &&" tt -> 0 "&&" }t
t{ s" =" tt -> 0 "=" }t
t{ s" *" tt -> 0 "*" }t
t{ s" *=" tt -> 0 "*=" }t
t{ s" 123405" tt -> 0 "decint" }t
t{ s" xyz" tt -> 0 "id" }t
t{ s" 0" tt -> 0 0 }t
t{ s" 123" tt -> 0 "decint" }t
t{ s" 9876L" tt -> 0 "decint" }t
t{ s" 5678l" tt -> 0 "decint" }t
t{ s" 0." tt -> 0 "floatconst" }t
t{ s" .0" tt -> 0 "floatconst" }t
t{ s" 1.0" tt -> 0 "floatconst" }t
t{ s" 3e1" tt -> 0 "floatconst" }t
t{ s" 1.0e-3" tt -> 0 "floatconst" }t
t{ s" .00034" tt -> 0 "floatconst" }t
t{ s" 3.14159" tt -> 0 "floatconst" }t
t{ s" 1e-3" tt -> 0 "floatconst" }t
t{ s" 2e+9" tt -> 0 "floatconst" }t
t{ s" 1234.567E+45" tt -> 0 "floatconst" }t

\ Additional non C tests

t{ s" program" tt -> 0 "program" }t
t{ s" PROGRAM" tt -> 0 "program" }t
t{ s" PrOGraM" tt -> 0 "program" }t
t{ s" $a" tt -> 0 "[abc]" }t
t{ s" $A" tt -> 0 "[abc]" }t
t{ s" $b" tt -> 0 "[abc]" }t
t{ s" $B" tt -> 0 "[abc]" }t
t{ s" $c" tt -> 0 "[abc]" }t
t{ s" $C" tt -> 0 "[abc]" }t
t{ s" $d" tt -> 0 "[def]" }t
t{ s" $D" tt -> 0 "[def]" }t
t{ s" $e" tt -> 0 "[def]" }t
t{ s" $E" tt -> 0 "[def]" }t
t{ s" $f" tt -> 0 "[def]" }t
t{ s" $F" tt -> 0 "[def]" }t
t{ s" $" tt -> 0 "xyz" }t
t{ s" $z" tt -> 0 "xyz" }t
t{ s" $y" tt -> 0 "xyz" }t
t{ s" $yyy" tt -> 0 "xyz" }t
t{ s" $xy" tt -> 0 "xyz" }t
t{ s" $xxxxyy" tt -> 0 "xyz" }t
t{ s" $yz" tt -> 0 "xyz" }t
t{ s" $yyyz" tt -> 0 "xyz" }t
t{ s" $xyz" tt -> 0 "xyz" }t
t{ s" $xxxxyyz" tt -> 0 "xyz" }t
t{ s" the" tt -> 0 "the" }t
t{ s" then" tt -> 0 "then" }t
t{ s" thence" tt -> 0 "thence" }t

create metas char $ c, char [ c, char ] c, char ( c, char ) c,
             char | c, char * c, char + c, char ? c, char - c,
             char # c, char { c, char } c, char \ c,
align

t{ metas char+ 13 tt -> 0 "meta" }t
t{ s" jj" tt -> 0 "escaped" }t
t{ metas 14 tt -> 0 "[meta]" }t
t{ s"   YZZ YYYZ" tt -> 0 "[escaped]" }t

t{ s" \" tt -> 0 0 }t       \ Not recognised

\ End of file
t{ s" " tt -> eof-tok }t
close-source

cr cr .( LexGen state transition table & file scanner tested.) cr
