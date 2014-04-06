(in-package :cl-user)

(defpackage :lol-re-tests
  (:use :cl :lol-re :fiveam)
  (:export #:run-tests))

(in-package :lol-re-tests)

(enable-read-macro-tokens)

(def-suite lol-re)
(in-suite lol-re)

(defun run-tests ()
  (let ((results (run 'lol-re)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basics
  (is (equal "a" (funcall (m~ "a") "a")))
  (is (equal "a" (m~ "a" "a"))))

