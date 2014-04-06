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
  * (wont do) do all the expansion at read-time, so that ((m~ "asdf") str) syntax be possible
  * usage of cl-interpol strings as regex-spec
  * (done) list of strings instead of just one string (auto joining)
  * ability to turn off some anaphoric bindings
  * convenient iterate macros
    * for iterating over all matches within a given string
    * for iterating over multiple strings with the same regexp

S~
--

1. Not only \1 ... \2 may be used in target strings, but also $($1) .. $($9)
   (and also named groups), a la cl-interpol

TODO:
  * creation of substituter, both target and replacement are plain strings


re-local
--------

The purpose of this macro is to tackle issues, that arise when multithreading.

```lisp
(re-local (all-the-variables like $1 $2 $a)
          (arising-from-use-of-m~)
          (are-declared-local-special)
          (inside-re-local))
```

So
```lisp
;; this is not thread-safe, as some other thread may corrupt $1 before PRINC gets executed
(and (m~ "foo(bar)") (princ $1))
```

But
```lisp
;; this is (supposedly) thread-safe, as all the relevant variables are implicitly
(re-local (and (m~ "foo(bar)") (princ $1)))
```

How it works: codewalks (with help of HU.DWIM.WALKER) the body, with M~ and S~
redefined as MACROLETs, with same expansion, but with side-effect
of telling RE-LOCAL, what variables they are going to initialize.

N.B.: If M~ was to be defined fully as read-time macro,
 then it's not possible to write RE-LOCAL even using code-walking,
since it's not possible (read: very hard and ugly) to read the form twice from a stream.
So, I won't define M~ to be read-time macro, at cost of sometimes being required to write FUNCALL.

Gotchas
-------

* potential racing conditions when multithreading (but, read re-local section)
