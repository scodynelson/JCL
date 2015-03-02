;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/backq.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    BACKQUOTE: Code Spice Lispified by Lee Schumacher.
;;;   		  (unparsing by Miles Bader)
;;;
(in-package "LISP")

(intl:textdomain "cmucl")

;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a		;the NIL flag is used only when a is NIL
;;;      T: [a] => a		;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a)
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;   \ car  ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|
;;;cdr \     ||                 |    T or NIL     |                |
;;;================================================================================
;;;  |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d])
;;;  NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a
;;;QUOTE or T|| LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC (a [d])
;;; APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC (a [d])
;;; NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC (a . d)
;;;  LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC (a [d])
;;;  LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d])
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead
;;; of ",@a)"

(defvar *backquote-count* 0  "How deep we are into backquotes")
(defvar *bq-comma-flag* '(|,|))
(defvar *bq-at-flag* '(|,@|))
(defvar *bq-dot-flag* '(|,.|))
(defvar *bq-vector-flag* '(|bqv|))
(defvar *bq-tokens*
  '(backq-comma backq-comma-at backq-comma-dot backq-list
    backq-list* backq-append backq-nconc backq-cons backq-vector))

;; This is the actual character macro.
(defun backquote-macro (stream ignore)
  (let ((*backquote-count* (1+ *backquote-count*)))
    (multiple-value-bind (flag thing)
			             (backquotify stream (read stream t nil t))
      (when (eq flag *bq-at-flag*)
	    (%reader-error stream (intl:gettext ",@ after backquote in ~S") thing))
      (when (eq flag *bq-dot-flag*)
	    (%reader-error stream (intl:gettext ",. after backquote in ~S") thing))
      (values (backquotify-1 flag thing)
              'list))))

(defun comma-macro (stream ignore)
  (unless (> *backquote-count* 0)
    (when *read-suppress*
      (return-from comma-macro nil))
    (%reader-error stream (intl:gettext "Comma not inside a backquote.")))
  (let ((c (read-char stream))
		(*backquote-count* (1- *backquote-count*)))
    (values (cond ((char= c #\@)
			       (cons *bq-at-flag* (read stream t nil t)))
			      ((char= c #\.)
			       (cons *bq-dot-flag* (read stream t nil t)))
			      (t
			       (unread-char c stream)
			       (cons *bq-comma-flag* (read stream t nil t))))
            'list)))

;;;
(defun expandable-backq-expression-p (object)
  (and (consp object)
       (let ((flag (car object)))
         (or (eq flag *bq-at-flag*)
             (eq flag *bq-dot-flag*)))))

;;; This does the expansion from table 2.
(defun backquotify (stream code)
  (cond ((atom code)
	     (cond ((null code)
	            (values nil
	                    nil))
	           ((or (consp code)
		            (symbolp code))
				;; Keywords are self evaluating. Install after packages.
				(values 'quote
						code))
	           (t
	            (values t
	                    code))))
		((or (eq (car code) *bq-at-flag*)
	         (eq (car code) *bq-dot-flag*))
	     (values (car code)
	             (cdr code)))
		((eq (car code) *bq-comma-flag*)
	     (comma (cdr code)))
		((eq (car code) *bq-vector-flag*)
	     (multiple-value-bind (dflag d)
	                          (backquotify stream (cdr code))
	       (values 'vector
	               (backquotify-1 dflag d))))
		(t
		 (multiple-value-bind (aflag a)
		                      (backquotify stream (car code))
	       (multiple-value-bind (dflag d)
	                            (backquotify stream (cdr code))
	         (when (eq dflag *bq-at-flag*)
		       (%reader-error stream (intl:gettext ",@ after dot in ~S") code))
	         (when (eq dflag *bq-dot-flag*)
		       (%reader-error stream (intl:gettext ",. after dot in ~S") code))
	         (cond ((eq aflag *bq-at-flag*)
		            (if (null dflag)
		                (if (expandable-backq-expression-p a)
                            (values 'append
                                    (list a))
                          (comma a))
		              (values 'append
			                  (cond ((eq dflag 'append)
				                     (cons a d))
				                    (t
				                     (list a
				                           (backquotify-1 dflag d)))))))
				   ((eq aflag *bq-dot-flag*)
					(if (null dflag)
					    (if (expandable-backq-expression-p a)
			                (values 'nconc
			                        (list a))
			              (comma a))
					  (values 'nconc
						      (cond ((eq dflag 'nconc)
							         (cons a d))
							        (t
							         (list a
							               (backquotify-1 dflag d)))))))
				   ((null dflag)
					(if (memq aflag '(quote t nil))
					    (values 'quote
					            (list a))
					  (values 'list
					          (list (backquotify-1 aflag a)))))
				   ((memq dflag '(quote t))
					(if (memq aflag '(quote t nil))
					    (values 'quote
					            (cons a d))
					  (values 'list*
					          (list (backquotify-1 aflag a)
					                (backquotify-1 dflag d)))))
				   (t
				    (setq a (backquotify-1 aflag a))
					(if (memq dflag '(list list*))
					    (values dflag
					            (cons a d))
					  (values 'list*
						      (list a
						            (backquotify-1 dflag d)))))))))))

;;; This handles the <hair> cases
(defun comma (code)
  (cond ((atom code)
	     (cond ((null code)
				(values nil
						nil))
	           ((or (numberp code)
	                (eq code 't))
				(values t
						code))
	           (t
	            (values *bq-comma-flag*
	                    code))))
		((and (eq (car code) 'quote)
		      (not (expandable-backq-expression-p (cadr code))))
		 (values (car code)
		         (cadr code)))
		((memq (car code) '(append list list* nconc))
		 (values (car code)
		         (cdr code)))
		((eq (car code) 'cons)
		 (values 'list*
		         (cdr code)))
		(t
		 (values *bq-comma-flag*
		         code))))

;;; This handles table 1.
(defun backquotify-1 (flag thing)
  (cond ((or (eq flag *bq-comma-flag*)
	         (memq flag '(t nil)))
	     thing)
		((eq flag 'quote)
		 (list 'quote
		       thing))
		((eq flag 'list*)
	     (cond ((and (null (cddr thing))
	                 (not (expandable-backq-expression-p (cadr thing))))
				(cons 'backq-cons thing))
		       ((expandable-backq-expression-p (car (last thing)))
	            (list 'backq-append
	                  (cons 'backq-list (butlast thing))
	                  (car (last thing))))
	           (t
				(cons 'backq-list* thing))))
		((eq flag 'vector)
		 (list 'backq-vector
		       thing))
		(t
		 (cons (cdr (assq flag '((cons . backq-cons)
								 (list . backq-list)
								 (append . backq-append)
								 (nconc . backq-nconc))))
			   thing))))


;;;; Magic backq- versions of builtin functions.

;;; Use synonyms for the lisp functions we use, so we can recognize backquoted
;;; material when pretty-printing

(defun backq-list (&rest args)
  args)
(defun backq-list* (&rest args)
  (apply #'list* args))
(defun backq-append (&rest args)
  (apply #'append args))
(defun backq-nconc (&rest args)
  (apply #'nconc args))
(defun backq-cons (x y)
  (cons x y))

(macrolet ((frob (b-name name)
	     `(define-compiler-macro ,b-name (&rest args)
		`(,',name ,@args))))
  (frob backq-list list)
  (frob backq-list* list*)
  (frob backq-append append)
  (frob backq-nconc nconc)
  (frob backq-cons cons))

(defun backq-vector (list)
  (declare (list list))
  (coerce list 'simple-vector))

;;;; BACKQ-INIT

(set-macro-character #\` #'backquote-macro)
(set-macro-character #\, #'comma-macro)
