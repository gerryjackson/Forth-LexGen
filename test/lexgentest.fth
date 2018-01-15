\ Load and run the test program for LexGen with regular expression input

: p
   cr .s key dup [char] q = swap [char] Q = or
   if ." Quit executed" cr quit then
;

[undefined] marker
[if] : drop-filetest ; : drop-lexgen ;
[else] marker drop-filetest marker drop-lexgen
[then]


1 [if]
   s" test/teststt.fth" r/o open-file  0=
   [if] close-file throw
        s" test/teststt.fth" delete-file throw
   [else]
      drop
   [then]
   s" src/lexgenloader.fth" included
   s" test/testtokens.fth" included
   s" test/testlexinput.fth" included
[then]

drop-lexgen

s" lib/ansify.fth" included
s" test/testtokens.fth" included
s" test/tester.fr" included
s" test/teststt.fth" included
s" src/filescanner.fth" included
\ s" test/test.txt" scan-file
s" test/testtarget.fth" included

drop-filetest

s" lib/ansify.fth" included
s" test/testtokens.fth" included
s" test/tester.fr" included
s" test/teststt.fth" included
s" src/sourcescanner.fth" included
\ s" test/test2.txt" included
s" test/testtarget2.fth" included
.s
