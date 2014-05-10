;;;; package.lisp

(defpackage #:lol-re
  (:use #:cl #:cl-read-macro-tokens #:defmacro-enhance #:iterate #:hu.dwim.walker)
  (:export #:m~ #:mr~ #:s~ #:re-local
	   #:enable-read-macro-tokens #:disable-read-macro-tokens))


