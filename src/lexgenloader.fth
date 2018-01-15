\ LexGenLoader.fth   Includes all necessary files to run LexGen, provided
\ for the user include

: version  s" 2.3.0" ;

cr cr .( Loading LexGen ) version type .(  ...) cr cr

\ -----------------------------------------------------------------------------
\ Included files

decimal

: p
   cr .s key dup [char] q = swap [char] Q = or
   if ." Quit executed" cr quit then
;

s" lib/ansify.fth" included
s" lib/xmini_oof.fth" included
s" lib/sets.fth" included
s" lib/shellsort.fth" included
s" src/syntaxtree.fth" included
s" src/transitiontable.fth" included
s" src/lexarrays.fth" included
s" src/savetables.fth" included
s" src/userinterface.fth" included

\ Regular expression compiler files

s" src/rgxstt.fth" included
s" src/rgxparser.fth" included



\ -----------------------------------------------------------------------------

cr .( LexGen ) version type .(  loaded successfully. ) .s
