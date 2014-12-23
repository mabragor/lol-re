;;; Taken from https://github.com/jschatzer/perlre/blob/master/perlre.lisp
(defpackage lol-re.syntax
  (:use cl)
  (:import-from defmacro-enhance
                defmacro!)
  (:import-from cl-syntax
                defsyntax)
  (:import-from alexandria
                symbolicate))

(in-package lol-re.syntax)

(defun segment-reader (strm ch n)
  "with m'' or s''' supress string interpolation, camel192"
  (if (> n 0)
    (let (chars)
      (do ((curr #1=(read-char strm) #1#))
        ((char= ch curr))
        (push curr chars))
      (if (char= ch #\')
        (cons (coerce (nreverse chars) 'string) (segment-reader strm ch (1- n)))
        (cons (with-input-from-string (s (coerce (nreverse chars) 'string)) (read s))
              (segment-reader strm ch (1- n)))))))

(define-symbol-macro regex
    `(if (zerop (length ,o!-mods))
         (car ,o!-args)
         (format nil "(?~a)~a" (remove #\g ,o!-mods) (car ,o!-args))))

(defmacro! subst-mode-ppcre-lambda-form (o!-args o!-mods)
  ``(lambda (,',g!str)
      ,(if (find #\g ,g!mods)
           `(ppcre:regex-replace-all ,,regex ,',g!str ,(cadr ,g!args))
           `(ppcre:regex-replace ,,regex ,',g!str ,(cadr ,g!args)))))

(defmacro! match-mode-ppcre-lambda-form (o!-args o!-mods)
  ``(lambda (,',g!str)
      (ppcre:scan-to-strings ,,regex ,',g!str)))

(defun mods (stm)
  "imsxg modifiers"
  (coerce (loop for c = (read-char stm)
                while (alpha-char-p c) collect c
                finally (unread-char c stm))
          'string))

(defun |#~-reader| (stream char numarg)
  (declare (ignore numarg))
  (print char)
  (let ((mode-char (read-char stream)))
    (case mode-char
      (#\m (match-mode-ppcre-lambda-form (segment-reader stream
                                                         (read-char stream)
                                                         1)
                                         (mods stream)))
      (#\s (subst-mode-ppcre-lambda-form (segment-reader stream
                                                         (read-char stream)
                                                         2)
                                         (mods stream)))
      (t (error "Unknown #~~ mode character")))))



(defun xx (l i)
  (case i
    (\` (first l))
    (& (second l))
    (\' (third l))))

(defmacro ifmatch ((test str) conseq &optional altern)
  `(multiple-value-bind (m a) (,test ,str) ; for match and array
     (eval `(if (plusp (length ,m))
              (let ((ml (ppcre:split (format nil "(~a)" ,m) ,',str :with-registers-p t :limit 3))) ;for match-list
                (let ,#1=(append
                           (mapcar #`(,(symbolicate "$" a1) (xx ml ',a1)) '(\` & \'))
                           (mapcar #`(,(symbolicate "$" a1) (aref ,a ,(1- a1))) (loop for i from 1 to (length a) collect i)))
                  (declare (ignorable ,@(mapcar #'car #1#)))
                  ,',conseq))
              ,',altern))))

(defmacro whenmatch ((test str) conseq &rest more-conseq)
  #"(whenmatch (#~m/"(b)(c)(d)(e)"/ "abcdef")
    (print |$`|)
    (print $2)
    (print $4))"#
  `(ifmatch (,test ,str)
     (progn ,conseq ,@more-conseq)))

(defsyntax lol-re-syntax
  (:merge :standard)
  (:dispatch-macro-character #\# #\g #'|#~-reader|))
