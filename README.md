lol-re
======

Tiny wrapper around CL-PPCRE, making usage of regexps more perly.
Inspired by let-over-lambda's #~m and #~s macro (http://www.letoverlambda.com)

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
The only differences are that interpolation is not possible for now, and you don't have
to write #?r in front of it. Also, you can only use double quotes as outer delimiters,
since doing otherwise confuses lisp-mode of Emacs a lot :)

When called with one argument (regexp), M~ expands into closure, which
accepts string. When called with same string repeatedly, it outputs
subsequent matches of the regexp on that string.
When called with different strings, behavior may be strange.
However, when called with :RESET keyword, the position counter inside the closure
is reset, so closure can be called now on some new string.

When called with two arguments (regexp and string), M~ expands into
application of a matching closure to that string, so the first match
of regexp on that string.

M~ also sets some anaphoric bindings (as seen in the example):
  * $0 is the whole match, $-0 and $+0 are the beginning and the end positions of whole match
  * $1 is the first group, $-1 and $+1 are the beginning and the end of the first group
  * ... and so on for all other groups
  * if a group was *named*, say, "foo", then also variables $FOO, $-FOO and $+FOO, with
    similar meaning. When generating symbol-name case of register is reversed, so
    for register named "fOo" symbols would be $|FoO|, $-|FoO| and $+|FoO|.

Since all those anaphoric bindings are global dynamic variables
  * first: they are equal to the ones relevant for the latest (in physical time) match performed.
  * second: this may be tricky when multithreading, but see RE-LOCAL macro below

System also defines two drivers for iterate: IN-MATCHES-OF and MATCHING

```lisp
(iter (for match in-matches-of "asdf" using (m~ "[a-z]([a-z])"))
      (collect `(,match ,$0 ,$1)))
(("as" "as" "s") ("df" "df" "f"))
```
As seen from the example, IN-MATCHES-OF iterates over all
matches of given regexp in a given string. Both string and regexp
are evaluated once-only.

In contrast, first example could be rewritten using MATCHING driver as follows:

```lisp
(with-open-file (out-file "out-file" :direction :output)
  (iter (for line in-file "in-file" using #'readline)
        (for match matching line using (m~ "(some)regexp(?<with>with)grouping(s)"))
	(format out-file #?"$($1) $($2) $($with) $($3)")))
```
but there are couple important things, which MATCHING does differently:
  * regexp is evaluated once-only, before the loop starts
  * even if the line didn't match regexp, FORMAT is still executed, printing line of NILs

So, MATCHING is more-or-less analogous to
```lisp
(let ((matcher (m~ "(some)regexp(?<with>with)grouping(s)")))
  (with-open-file (out-file "out-file" :direction :output)
    (iter (for line in-file "in-file" using #'readline)
          (funcall matcher line)
          (format out-file #?"$($1) $($2) $($with) $($3)"))))
```

TODO:
  * (done) creation of scanner, when regex-spec is just plain string
  * (wont do) do all the expansion at read-time, so that ((m~ "asdf") str) syntax be possible
  * usage of cl-interpol strings as regex-spec
  * (done) list of strings instead of just one string (auto joining)
  * ability to turn off some anaphoric bindings
  * (done) convenient iterate macros
    * (done) for iterating over all matches within a given string
    * (done) for iterating over multiple strings with the same regexp
  * ability to use only #?r syntax on implementations not supported by CL-READ-MACRO-TOKENS

For more usage patterns, see tests.lisp file and use-cases.lisp.
use-cases.lisp was assembled by grepping of some quicklisp-available libs
and rewriting CL-PPCRE-using pieces with help of M~ and S~.

S~
--

For now, replacing only can replace first occurence of the match.
But, still, it's not needed to escape all those backslashes.

```lisp
LOL-RE> (s~ "(\d{4})-(\d{2})-(\d{2})" "\3/\2/\1" "2014-04-07")
07/04/2014
```

When called with just 2 arguments, generates replacer closure

```lisp
LOL-RE> (funcall (s~ "(\d{4})-(\d{2})-(\d{2})" "\3/\2/\1") "2014-04-07")
07/04/2014
```


TODO:
  * (done) creation of substituter, both target and replacement are plain strings, no named groups allowed
  * named groups are allowed in target
  * named groups are also allowed in replacement
  * cl-interpol #?r strings
  * lists are allowed instead of plain strings
  * G symbol switch to do all possible replacements


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
