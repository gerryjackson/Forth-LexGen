\ Not sure where I found this on the internet, thanks to whoever wrote it

\ Non-recursive shell-sort, employing a hibbard-sequence.
\ predictable stack-depth, independent of sort data.

\ This sort is called "generic" because it contains no knowledge of the
\ data to sort. This knowledge is coded into the compare and swap methods.
\ For example, an indexed sort (by not rearranging the sort data,
\ but an index with pointer to sort elements) is just as possible as
\ a direct sort.
\ Sort data requires just to be addressable by index 0..elements-1,
\ such as array indices.

\ Adapted to use Mini-OOF classes instead of deferred words.

\ Sub-class the SortBase class and define the methods for particular
\ data types and arrays

[defined] [-dev-] [if] .( Loading ShellSort.fth...) cr [then]

object class
   class-method sortswap      ( u1 u2 class -- )
   class-method sortcompare   ( u1 u2 class -- flag )
end-class SortBase

: (sort)    ( inc i class -- )
   >r
   begin
      2dup + over r@ sortswap
      over -
      dup 0< 0=
   while
      2dup + over r@ sortcompare
      0=
   until then
   2drop r> drop
;

: ShellSort  ( class u -- )     \ u is the number of elements to sort
   dup >r                           ( -- class u )    ( R: -- u )
   begin
      2/ dup                        ( -- class u2 u2 )
   while
      1- 1 or                       ( -- class u2' )
      dup r@ - negate ?dup          ( -- class u2' u3 )
      if
         0
         do                         ( -- class u2' )
            2dup i + i              ( -- class u2' class i1 i2 )
            rot sortcompare         ( -- class u2' f )
            if
               2dup i               ( -- class u2' class inc i )
               rot (sort)           ( -- class u2' )
            then
         loop
      then
   repeat
   2drop r> drop
;

[defined] [-dev-] [if] .( ShellSort.fth loaded. ) .s [then]

\ ------------------------------------------------------------------------------
