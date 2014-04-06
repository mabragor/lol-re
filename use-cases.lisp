
(defun ip-addr-p (ip)
  (if~ ("^(\d+)\.(\d+)\.(\d+)\.(\d{1,3})$" ip)
       (and (< (parse-integer $1) 256) ; automatic binding of 'magic' group variables
	    (< (parse-integer $2) 256)
	    (< (parse-integer $3) 256)
	    (< (parse-integer $4) 256))))

(split "\n" asdf) ; no escaping when specifying splitting-regexp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro-enhance::def-*!-symbol-p c)
  (defun parse-c!-symbol (sym)
    (if~ ("^C!-([^-]+)(.*)" (string sym))
	 (values (intern (concatenate 'string "C!-" $1))
		 (if (string= "" $2)
		     nil
		     (subseq $2 1))))))

(defun name-has-dot? (n)
  (if~ ("\\." (string n))))

(let ((rt (if~ ("([\d\.]+) seconds of real" trc)
	       (parse-integer (s~ "\." "" $1))))
      (bc (if~ ("([\d\,]+) bytes" trc)
	       (parse-integer (s~ "\," "" $1)))))
  ....)

;; it would be even more cool to be able to write like this:
(let ((rt (if ((m~ "([\d\.]+) seconds of real") trc)
	      (parse-integer (s~ ("\." "" g) $1))))
      (bc (and ((m~ "([\d\,]+) bytes") trc) ; or even like this
	       (parse-integer ((s~ "\," "" g) $1))))) ; another plausible variant
  ....)

(labels ((parse-single-year (ugly-date-string)
	   (if (m~ "^([0-9]{4,4})$" ugly-date-string) ; 3 argument shortcut
	       (encode-universal-time 0 0 0 1 1 (parse-number:parse-number $1)))))
  ...)

(defparameter *color-scanner* (m~ ("[0-9]{1,2}(,[0-9]{1,2}){0,1}||||" p)))

(defun extract-color (string)
  (if (funcall *color-scanner* string)
      (let* ((message (subseq string $+0))
	     (color-code $0)
	     (color-code (or (mapcar (m~ "[0-9]{1,2}") color-code)
			     (list (m~ "|||" color-code))))
	     ...)
	...)
      string))



