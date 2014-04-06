lol-re
======

Tiny wrapper around CL-PPCRE,
to make usage of regexps more perly in the spirit of let-over-lambda (http://www.letoverlambda.com)

This package introduces two car-reader-macro (see CL-READ-MACRO-TOKENS) M~ and S~.
M~ is for matching, while S~ is for substitution
(in direct analogy with Perl's '=~ m//' and ' =~ s///' idioms).

M~
--

Basic example:

```lisp
(with-open-file (out-file "out-file" :direction :output)
  (iter (for line in-file "in-file" using #'readline)
        (and (m~ "(some)regexp(?<with>with)grouping(s)" line)
             (format out-file #?"$($1) $($2) $($with) $($3)"))))
```

Syntax of string in M~ is the same as in #?r cl-interpol's reader macro.
Only so far interpolations are not possible, but you don't have to escape backslashes!

TODO:
  * (done) creation of scanner, when regex-spec is just plain string
  * do all the expansion at read-time, so that ((m~ "asdf") str) syntax be possible
  * usage of cl-interpol strings as regex-spec
  * (done) list of strings instead of just one string (auto joining)
  * ability to turn off some anaphoric bindings

S~
--

1. Not only \1 ... \2 may be used in target strings, but also $($1) .. $($9)
   (and also named groups), a la cl-interpol

TODO:
  * creation of substituter, both target and replacement are plain strings

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
