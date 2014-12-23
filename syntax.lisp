;;; Taken from https://github.com/jschatzer/perlre/blob/master/perlre.lisp
;; Authors: Doug Hoyte, jschatzer and André Miranda.
(defpackage lol-re.syntax
  (:use cl)
  (:import-from defmacro-enhance
                defmacro!)
  (:import-from cl-syntax
                defsyntax)
  (:import-from alexandria
                symbolicate)
  (:export lol-re-syntax))
(in-package lol-re.syntax)

(defun segment-reader (stream ch n)
  "with m'' or s''' supress string interpolation, camel192"
  (if (> n 0)
    (let (chars)
      (do ((curr #1=(read-char stream) #1#))
          ((char= ch curr))
        (push curr chars))
      (if (char= ch #\')
          (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (1- n)))
          (cons (with-input-from-string (s (coerce (nreverse chars) 'string))
                  (read s))
                (segment-reader stream ch (1- n)))))))

(define-symbol-macro regex
    `(if (zerop (length ,o!-mods))
         (car ,o!-args)
         (format nil "(?~a)~a" (remove #\g ,o!-mods) (car ,o!-args))))

(defmacro! subst-mode-ppcre-lambda-form (o!-args o!-mods)
  ``(lambda (,',g!-str)
      ,(if (find #\g ,o!-mods)
           `(ppcre:regex-replace-all ,,regex ,',g!-str ,(cadr ,o!-args))
           `(ppcre:regex-replace ,,regex ,',g!-str ,(cadr ,o!-args)))))

(defmacro! match-mode-ppcre-lambda-form (o!-args o!-mods)
  ``(lambda (,',g!-str)
      (ppcre:scan-to-strings ,,regex ,',g!-str)))

(defun mods (stream)
  "imsxg modifiers"
  (coerce (loop for c = (read-char stream)
                while (alpha-char-p c) collect c
                finally (unread-char c stream))
          'string))

(defun |#~-reader| (stream char numarg)
  (declare (ignore char numarg))
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

(defmacro ifmatch ((test str) then &optional else)
  "Checks for the existence of group-capturing regex (in /for(bar)baz/, bar is capured) in the TEST and bind $1, $2, $n vars to the captured regex. Obviously, doesn't work with runtime regexes"
  (let* ((regexp (second (third test)))
         (how-many-$-vars (when (stringp regexp)
                            (let ((regex-paretheses
                                   (ppcre:all-matches-as-strings "\\((.*?)\\)"
                                                                 regexp)))
                              (print regex-paretheses)
                              (length regex-paretheses))))
         ($-vars-let-form
          (append '((|$`| (XX ML '|`|)) ($& (XX ML '&)) (|$'| (XX ML '|'|)))
                  (when how-many-$-vars
                    (mapcar (lambda (var-num)
                              `(,(symbolicate "$"
                                              (write-to-string var-num))
                                 (aref arr ,(1- var-num))))
                            (loop for i from 1 to how-many-$-vars collect i))))))
    `(multiple-value-bind (matches arr) (,test ,str)
       (if (plusp (length matches))
           (let* ((match-list (ppcre:split (format nil "(~a)" matches)
                                           ,str :with-registers-p t :limit 3))
                  (declare (ignorable ,@(mapcar #'car $-vars-let-form)))
                  ,then))
           ,else))))

(defmacro whenmatch ((test str) &body forms)
  "(whenmatch (#~m/\"(b)(c)(d)(e)\"/ \"abcdef\")
     (print |$`|)
     (print $2)
     (print $4))"
  `(ifmatch (,test ,str)
     (progn ,@forms)))

(defsyntax lol-re-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\~ #'|#~-reader|))
