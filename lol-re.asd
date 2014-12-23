;;;; lol-re.asd

(asdf:defsystem #:lol-re
  :version "0.1"
  :serial t
  :description "Small set of wrappers around CL-PPCRE in spirit of Let Over Lambda."
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-ppcre
               #:cl-interpol
               #:cl-read-macro-tokens
               #:cl-syntax
               #:defmacro-enhance
               #:alexandria
               #:iterate
               #:hu.dwim.walker)
  :components ((:file "package")
               (:file "lol-re")))


(defsystem :lol-re-tests
  :description "Tests for LOL-RE."
  :licence "GPL"
  :depends-on (#:lol-re #:fiveam #:iterate)
  :serial t
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :lol-re))))
  (load-system :lol-re)
  (funcall (intern "RUN-TESTS" :lol-re-tests)))
