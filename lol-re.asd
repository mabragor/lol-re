;;;; lol-re.asd

(defsystem "lol-re"
  :version "0.1"
  :serial t
  :description "Small set of wrappers around CL-PPCRE in spirit of Let Over Lambda."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on ("cl-ppcre"
               "cl-interpol"
               "cl-read-macro-tokens"
               "named-readtables"
               "defmacro-enhance"
               "alexandria"
               "iterate"
               "hu.dwim.walker")
  :components ((:file "syntax")
               (:file "package")
               (:file "lol-re"))
  :in-order-to ((test-op (test-op "lol-re/tests"))))


(defsystem "lol-re/tests"
  :description "Tests for LOL-RE."
  :licence "GPL"
  :depends-on ("lol-re" "fiveam" "iterate")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :lol-re-tests :run-tests)))
