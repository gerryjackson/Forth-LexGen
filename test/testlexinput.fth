\ ------------------------------------------------------------------------------
\ The test input file for LexGen
\ ------------------------------------------------------------------------------
\ Define the output and the scanner file names

s" test/teststt.fth" setOutputFile

no-scanner      \ Uncomment this line to get state transition tables only

\ formatted       \ Comment out this line to get unformatted output

\ -----------------------------------------------------------------------------
\ Essential to define the size of the character set. Not advisable to set
\ it to more than 255 i.e. 8 bit character set

127 setMaxChar

\ -----------------------------------------------------------------------------
\ C Keywords and symbols
case-sensitive

\ **********************

regex letter         [a-zA-Z]
regex digit          [0-9]
regex digit1to9      [1-9]
regex identifier     ({letter}|_)({letter}|{digit}|_)*
regex integer        {digit1to9}{digit}*(l|L)?

\ Floating point constants
regex digits         {digit}+
regex dotted-digits  {digits}.|{digits}.{digits}|.{digits}
regex exponent       (e|E)(\+|\-)?{digits}
regex float-const    {digits}{exponent}|{dotted-digits}{exponent}?

"id"           ==>   {identifier}
"decint"       ==>   {integer}
"floatconst"   ==>   {float-const}
"ws"           ==>   [\s\t\n\r]+
"void"         ==>   void
"auto"         ==>   auto
"static"       ==>   static
"extern"       ==>   extern
"register"     ==>   register
"typedef"      ==>   typedef
"char"         ==>   char
"float"        ==>   float
"double"       ==>   double
"int"          ==>   int
"short"        ==>   short
"long"         ==>   long
"unsigned"     ==>   unsigned
"struct"       ==>   struct
"union"        ==>   union
"enum"         ==>   enum
"if"           ==>   if
"else"         ==>   else
"while"        ==>   while
"do"           ==>   do
"for"          ==>   for
"switch"       ==>   switch
"break"        ==>   break
"continue"     ==>   continue
"return"       ==>   return
"goto"         ==>   goto
"case"         ==>   case
"default"      ==>   default
"sizeof"       ==>   sizeof
";"            ==>   ;
"{"            ==>   \{
"}"            ==>   \}
","            ==>   ,
":"            ==>   :
"="            ==>   =
"("            ==>   \(
")"            ==>   \)
"["            ==>   \[
"]"            ==>   \]
"*"            ==>   \*
"."            ==>   .
"->"           ==>   \->
"++"           ==>   \+\+
"--"           ==>   \-\-
"&"            ==>   &
"-"            ==>   \-
"!"            ==>   !
"~"            ==>   ~
"/"            ==>   /
"%"            ==>   %
"+"            ==>   \+
"<<"           ==>   <<
">>"           ==>   >>
"<"            ==>   <
"<="           ==>   <=
">="           ==>   >=
">"            ==>   >
"=="           ==>   ==
"!="           ==>   !=
"^"            ==>   ^
"|"            ==>   \|
"&&"           ==>   &&
"||"           ==>   \|\|
"?"            ==>   \?
"+="           ==>   \+=
"-="           ==>   \-=
"*="           ==>   \*=
"/="           ==>   /=
"%="           ==>   %=
">>="          ==>   >>=
"<<="          ==>   <<=
"&="           ==>   &=
"^="           ==>   ^=
"|="           ==>   \|=

\ Additonal tests (not C)

\ Case insensitivity and negated character class

case-insensitive
"program"      ==>   program
"[abc]"        ==>   $[abc]
"[def]"        ==>   $[^\0-CG-`\{-\d{127}]
case-sensitive
"xyz"          ==>   $(x*y+)?z?
"the"          ==>   the
"then"         ==>   then
"thence"       ==>   thence
"meta"         ==>   \[\]\(\)\|\*\+\?\-\#\{\}\\
"escaped"      ==>   \d{106}\x{6A}
"[meta]"       ==>   $[\[\]\(\)\|\*\+\?\-\#\{\}\\]+
"[escaped]"    ==>   [\d{89}\x{5A}\s]+

\ The call to generate scanner data

lexgen

\ -----------------------------------------------------------------------------
