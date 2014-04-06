;;;; package.lisp

(defpackage #:lol-re
  (:use #:cl #:cl-read-macro-tokens #:defmacro-enhance #:iterate)
  (:export #:m~ #:s~ #:enable-read-macro-tokens #:disable-read-macro-tokens))

