;;;; lol-re.lisp

(in-package #:lol-re)

;;; "lol-re" goes here. Hacks and glory await!

(defun mk-scan-iter (scanner)
  (let ((pos 0))
    (lambda (str)
      (if (eq str :reset)
	  (progn (setf pos 0)
		 t)
	  (multiple-value-bind (match-start match-end reg-starts reg-ends)
	      (cl-ppcre:scan scanner str :start pos)
	    (if match-start
		(progn (setf pos match-end)
		       (values match-start match-end reg-starts reg-ends))))))))

(defun string-reverse-case (str)
  (iter (for char in-string str)
	(if (upper-case-p char)
	    (collect (char (string-downcase char) 0) into res)
	    (collect (char (string-upcase char) 0) into res))
	(finally (return (coerce res 'string)))))
	    

(defun list-o-syms (name)
  (let ((name (string-reverse-case (format nil "~a" name))))
    (list (intern (concatenate 'string "$" name))
	  (intern (concatenate 'string "$-" name))
	  (intern (concatenate 'string "$+" name)))))

(defun define-reg-vars (registers)
  (flet ((frob (reg)
	   `(progn (defvar ,reg nil)
		   (declaim (special ,reg)))))
    (let (res
	  (count 0))
      (dolist (reg registers)
	(incf count)
	(setf res (nconc (mapcar #'frob (list-o-syms count)) res))
	(if reg
	    (setf res (nconc (mapcar #'frob (list-o-syms reg)) res))))
      (setf res (nconc (mapcar #'frob (list-o-syms "0")) res))
      (nreverse res))))

(defun bind-regs (registers)
  (let (res
	(count 0))
    (flet ((frob (reg)
	     (mapcan #'list
		     (list-o-syms reg)
		     `((subseq str (aref reg-starts ,count) (aref reg-ends ,count))
		       (aref reg-starts ,count)
		       (aref reg-ends ,count)))))
      (dolist (reg registers)
	(push `(setf ,@(frob (1+ count))) res)
	(if reg
	    (push `(setf ,@(frob reg)) res))
	(incf count))
      (push `(setf ,@(mapcan #'list
			     (list-o-syms 0)
			     `((subseq str match-start match-end)
			       match-start
			       match-end)))
	    res)
      (nreverse res))))

(defun clear-regs (registers)
  (let (res
	(count 0))
    (flet ((frob (reg)
	     (mapcan #'list
		     (list-o-syms reg)
		     `(nil nil nil))))
      (dolist (reg registers)
	(push `(setf ,@(frob (1+ count))) res)
	(if reg
	    (push `(setf ,@(frob reg)) res))
	(incf count))
      (push `(setf ,@(mapcan #'list
			     (list-o-syms 0)
			     `(nil nil nil)))
	    res)
      (nreverse res))))

(defun lol-re-literal-string-reader (string-reader)
  (lambda (stream token)
    (with-macro-character (#\" string-reader)
      (progn (read-list-new stream token)))))

(defun lol-re-string-reader (cl-interpol::*stream* char)
  (declare (ignore char))
  (let ((cl-interpol::*start-char* #\")
	(cl-interpol::*term-char* #\")
	(cl-interpol::*pair-level* 0)
	(cl-interpol::*inner-delimiters* nil)
	cl-interpol::*saw-backslash*
	cl-interpol::*readtable-copy*)
    (prog1 (cl-interpol::inner-reader t nil nil nil)
      (cl-interpol::read-char*))))

(defmacro! with-re-reader-context (&body body)
  `(let ((,g!-string-reader (get-macro-character #\")))
     (with-macro-character (#\" #'lol-re-string-reader)
       (read-macrolet ((,e!-ls (lol-re-literal-string-reader ,g!-string-reader))
		       (,e!-literal-string (lol-re-literal-string-reader ,g!-string-reader)))
	 ,@body))))

(defmacro ensure-correct-regex-spec (regex-spec-var)
  `(macrolet ((myerr ()
		`(error "Sorry, only literal strings are supported as regex-specs for now.")))
     (cond ((stringp ,regex-spec-var) nil)
	   ((consp ,regex-spec-var) (if (remove-if #'stringp ,regex-spec-var)
					(myerr)
					(setf ,regex-spec-var (format nil "~{~a~}" ,regex-spec-var))))
	   (t (myerr)))))

(defmacro! with-scanner ((scan-var reg-var o!-regex-spec) &body body)
  `(multiple-value-bind (,scan-var ,reg-var)
      (let ((cl-ppcre::*allow-named-registers* t))
	(cl-ppcre:create-scanner ,o!-regex-spec))
     (when (not ,reg-var)
       (setf ,reg-var (make-list (length
				  (remove-if-not (lambda (x)
						   (eq x :register))
						 (alexandria:flatten (cl-ppcre:parse-string ,o!-regex-spec)))))))
     ,@body))


(defmacro!! m~ (regex-spec &optional (argument nil argument-p))
    `(,(slot-value obj 'cl-read-macro-tokens::name)
       ,(with-re-reader-context
	 (read stream t nil t))
       ,@(read-list-old stream token))
  (ensure-correct-regex-spec regex-spec)
  (with-scanner (scanner register-names regex-spec)
    (let ((pre-scanner-lambda (mk-scan-iter scanner)))
      (let ((scanner-code `(progn ,@(define-reg-vars register-names)
				  (lambda (str)
				    (multiple-value-bind (match-start match-end reg-starts reg-ends)
					(funcall ,pre-scanner-lambda str)
				      (declare (ignorable reg-starts reg-ends))
				      (if match-start
					  (if (eq match-start 't)
					      (progn ,@(clear-regs register-names)
						     t)
					      (progn ,@(bind-regs register-names)
						     ,(intern "$0")))
					  (progn ,@(clear-regs register-names)
						 nil)))))))
	(if (not argument-p)
	    scanner-code
	    `(funcall ,scanner-code ,argument))))))

(defvar *re-local-vars*)      
				  
(defmacro re-local (&body body &environment env)
  (let (*re-local-vars*)
    (declare (special *re-local-vars*))
    (macroexpand-all `(macrolet ((m~ (&whole form regex-spec &optional argument &environment env)
				   (ensure-correct-regex-spec regex-spec)
				   (with-scanner (scanner reg-vars regex-spec)
				     (setf *re-local-vars*
					   (nconc (iter (for reg in reg-vars)
							(for count from 1)
							(appending (list-o-syms count) into res)
							(if reg
							    (appending (list-o-syms reg) into res))
							(finally (return (append (list-o-syms 0)
										 res))))
						  *re-local-vars*))
				     (funcall (macro-function 'm~) form env))))
			,@body)
		     env)
    (setf *re-local-vars* (remove-duplicates *re-local-vars*))
    `(let ,*re-local-vars*
       (declare (special ,@*re-local-vars*))
       ,@body)))
