lol-re
======

Tiny wrapper around CL-PPCRE,
to make usage of regexps more perly in the spirit of let-over-lambda (http://www.letoverlambda.com)

This package introduces two car-reader-macro (see CL-READ-MACRO-TOKENS) M~ and S~.
M~ is for matching, while S~ is for substitution
(in direct analogy with Perl's '=~ m//' and ' =~ s///' idioms).

M~
--

What's new w.r.t combination of CL-PPCRE + CL-INTERPOL + RX:
1. Not only $1 .. $9 are bound, but also $-1 ... $-9, $+1 ... $+9, and
   also variables for named capture groups
2. Syntax duality:  1 argument - scanner closure,
   2 arguments - application of closure, returning matched substring
3. can specify attributes such as x g p e i conveniently
4. auto-concatenation of multiple strings (to ease write of regexp on
                                           multiple lines)

Plan of generalization:
1. First version: only literal strings are supported, errors on everything else
2. cl-interpol strings are supported, *but* group capturing information must not
   depend on interpolated pieces.
3. Optimizations: abilities to turn some anaphoric bindings off.

S~
--

1. Not only \1 ... \2 may be used in target strings, but also $($1) .. $($9)
   (and also named groups), a la cl-interpol

re-local
--------

How it works: codewalking of the body, with M~ and S~ substituted by
some other macro, whose purpose is to 'tell' RE-LOCAL, which variables are about
to be bound in them. Then, knowing those variables, it just LET-SPECIALs them.

Gotchas
-------

Of course, binding global dynamical variables (which $0, $1 and so on are) is not thread-safe.
When writing simple one-time functions (analog of Perl one-liners), or prototyping,
this is almost never a concern, however, at the later stages of development you
can simply enclose lol-re using parts of code into RE-LOCAL, which rebinds
all relevant dynamical variables locally, thus making them per-thread
(according to bordeaux-threads model).
