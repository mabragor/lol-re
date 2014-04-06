(in-package :cl-user)

(defpackage :lol-re-tests
  (:use :cl :lol-re :fiveam #:iterate)
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
  (is (equal "a" (m~ "a" "a")))
  (is (equal nil (funcall (m~ "a") "b")))
  (is (equal nil (m~ "a" "b"))))

(test iter
  (let ((re-iter (m~ "a([0-9])")))
    (is (equal '(0 1 2 3 4 5 6 7 8 9)
	       (iter (while (funcall re-iter "a0a1a2a3a4a5a6a7a8a9"))
		     (collect (parse-integer $1)))))))

(test named-groups
  (is (equal '("2014" "04" "06" 5 7)
	     (and (m~ "(?<y>\d{4})-(?<m>\d{2})-(?<d>\d{2})" "2014-04-06")
		  (list $y $m $d $-m $+m)))))
