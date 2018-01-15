\ Test tokens - user defined tokens for C

variable tokenval

: token ( "<spaces>name" -- ) ( use: token name ) ( name: -- n )
   tokenval @ constant
   1 tokenval +!
;

1 tokenval !

token "void"
token "auto"
token "static"
token "extern"
token "register"
token "typedef"
token "char"
token "float"
token "double"
token "int"
token "short"
token "long"
token "unsigned"
token "struct"
token "union"
token "enum"
token "if"
token "else"
token "while"
token "do"
token "for"
token "switch"
token "break"
token "continue"
token "return"
token "goto"
token "case"
token "default"
token "sizeof"
token ";"
token "{"
token "}"
token ","
token ":"
token "="
token "("
token ")"
token "["
token "]"
token "*"
token "."
token "->"
token "++"
token "--"
token "&"
token "-"
token "!"
token "~"
token "/"
token "%"
token "+"
token "<<"
token ">>"
token "<"
token "<="
token ">="
token ">"
token "=="
token "!="
token "^"
token "|"
token "&&"
token "||"
token "?"
token "+="
token "-="
token "*="
token "/="
token "%="
token ">>="
token "<<="
token "&="
token "^="
token "|="
token "id"
token "decint"
token "floatconst"
token "ws"

\ Additional non C tokens

token "program"
token "[abc]"
token "[def]"
token "xyz"
token "the"
token "then"
token "thence"
token "meta"
token "escaped"
token "[meta]"
token "[escaped]"

\ ------------------------------------------------------------------------------