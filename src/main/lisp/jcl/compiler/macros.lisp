;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; Define-Compiler-Macro

;; Define-Symbol-Macro
#|
(defun %define-symbol-macro (symbol expansion)
  (%set-symbol-macro symbol (make-symbol-macro expansion))
  symbol)

(defmacro define-symbol-macro (name expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-symbol-macro ',name ',expansion)))
|#
;; Declaim
#|
(defmacro declaim (declaration-specifier)
  (declare (system::%java-class-name "jcl.compiler.functions.Declaim"))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (proclaim ',declaration-specifier)))
|#
;; Defconstant, Defparameter, Devvar
#|
(defmacro defvar (symbol &optional (init-form nil) documentation)
  (declare (system::%java-class-name "jcl.compiler.functions.Defvar"))
  `(progn
     (system::%set-special ',symbol t)
     (unless (boundp ',symbol) (setq ,symbol ,init-form))))

(defmacro defparameter (symbol init-form &optional documentation)
  (declare (system::%java-class-name "jcl.compiler.functions.Defparameter"))
  `(progn
     (system::%set-special ',symbol t)
     (setq ,symbol ,init-form)))

(defmacro defconstant (symbol init-form &optional documentation)
  (declare (system::%java-class-name "jcl.compiler.functions.Defconstant"))
  `(progn
     (system::%set-special ',symbol t)
     (system::%set-constant ',symbol nil)
     (setq ,symbol ,init-form)
     (system::%set-constant ',symbol t)))
|#
;; Destructuring-Bind

;; Psetq
#|
(defun psetq-bindings (pairs)
  (if (null pairs)
      nil
    (cons `(,(gensym (symbol-name (first pairs))) ,(second pairs))
          (psetq-bindings (cddr pairs)))))

(defun map-pairs (pairs bindings)
  (if (null pairs)
      nil
    (cons (first pairs)
          (cons (caar bindings)
                (map-pairs (rest (rest pairs)) (rest bindings))))))

(defun psetq-aux (pairs)
  (let ((bindings (psetq-bindings pairs)))
    `(let ,bindings
       (setq ,@(map-pairs pairs bindings)))))

(defmacro psetq (&rest pairs)
  (declare (system::%java-class-name "lisp.common.function.Psetq"))
  (when pairs
    (unless (evenp (length pairs))
      (error "PSETQ requires pairs of arguments, ~S" pairs))
    (psetq-aux pairs)))
|#
;; Return

(defmacro return (&optional result)
  (declare (system::%java-class-name "jcl.compiler.functions.Return"))
  `(return-from nil ,result))

;; When, Unless

(defmacro when (test-form &rest body)
  (declare (system::%java-class-name "jcl.compiler.functions.When"))
  (if (cdr body)
      `(if ,test-form (progn ,@body))
    `(if ,test-form ,(car body))))

(defmacro unless (test-form &rest body)
  (declare (system::%java-class-name "jcl.compiler.functions.Unless"))
  (if (cdr body)
      `(if (not ,test-form) (progn ,@body))
    `(if (not ,test-form) ,(car body))))

;; Cond

(defmacro cond (&rest clauses)
  (declare (system::%java-class-name "jcl.compiler.functions.Cond"))
  (if (null clauses)
      nil
    (let ((clause (car clauses)))
	  (when (atom clause)
	    (error "COND clause is not a list: ~S" clause))
	  (let ((test (car clause))
	        (forms (cdr clause)))
	    (if (null forms)
	        (let ((result (gensym)))
			  `(let ((,result ,test))
			     (if ,result
			         ,result
			       (cond ,@(cdr clauses)))))
          `(if ,test
	           (progn ,@forms)
	         (cond ,@(cdr clauses))))))))

#|
(defmacro cond (whole env)
  (declare (system::%java-class-name "jcl.compiler.functions.Cond"))
  (let ((clause-set (cdr whole)))
    (let ((current-clause (car clause-set)))
      (if (and clause-set (car clause-set))
          (progn
	        (if (cdr clause-set)
	            `(if ,(car current-clause)
	                 (progn ,@(cdr current-clause))
	               ,(if (cdr clause-set)
	                    (progn
	                      `(cond ,@(cdr clause-set)))))
	          (let ((arg (gensym))
	                (first (car current-clause))
	                (rest (cdr current-clause)))
	            (if rest
	                `(if ,first (progn ,@rest))
	              `,first))))))))
|#
;; And, Or

(defmacro and (&rest forms)
  (declare (system::%java-class-name "jcl.compiler.functions.And"))
  (cond ((null forms) t)
		((null (cdr forms)) (car forms))
		(t
		 `(if ,(car forms)
		      (and ,@(cdr forms))
		    nil))))

(defmacro or (&rest forms)
  (declare (system::%java-class-name "jcl.compiler.functions.Or"))
  (cond ((null forms) nil)
		((null (cdr forms)) (car forms))
		(t
		 (let ((result (gensym)))
		   `(let ((,result ,(car forms)))
		      (if ,result
			      ,result
			    (or ,@(cdr forms))))))))

;; Case,Ccase,Ecase || Typecase, Ctypecase, Etypecase
#|
(defun parse-normal-clause (clause key-fn key-test)
  (when clause
    (if (not (listp clause))
        (error "CASE error: clause must be a list, ~S" clause)
      (let ((key-form (car clause))
            (forms (cdr clause)))
        (when key-form ;; if the key-form is NIL, drop the entry
          (unless (listp key-form)
            (setq key-form (list key-form)))
          `(if (or ,@(system::%mapcar #'(lambda (key) `(,key-fn ,key-test ',key)) key-form))
               (progn ,@forms)))))))

(defun handle-most-clauses (clauses key-fn key-test last-clause-callback)
  ;; it's a boo-boo if there is a t or otherwise in other than the last clause
    (let ((key-form (car (car clauses))))
      (when (and (rest clauses) (or (eq key-form t) (eq key-form 'otherwise)))
        (error "T and OTHERWISE are not permitted in other than the last clause"))
      ;; handle the clauses
      (let ((pnc (parse-normal-clause (car clauses) key-fn key-test)))
        (if pnc
          (if (rest clauses)
            (append pnc (list (handle-most-clauses (rest clauses) key-fn key-test last-clause-callback)))
            (funcall last-clause-callback (car clauses)))
          (handle-most-clauses (rest clauses) key-fn key-test last-clause-callback)))))

(defun handle-normal-clauses (clauses key-fn key-test)
  (handle-most-clauses clauses key-fn key-test
    ;; this is the last clause processor
    #'(lambda (last-clause)
       (let* ((key-form (car last-clause)))
         (if (or (eq key-form t) (eq key-form 'otherwise))
           `(progn ,(car (cdr last-clause)))
           (parse-normal-clause last-clause key-fn key-test))))))

(defun handle-error-clauses (clauses key-fn key-test typecase-p)
  (handle-most-clauses clauses key-fn key-test
    ;; this is the last clause processor
    #'(lambda (last-clause)
       (let* ((key-form (car last-clause)))
         (when (and (or (eq key-form t) (eq key-form 'otherwise)) (not typecase-p))
           (error "T and OTHERWISE are not permitted in ECASE"))
         ;; handle the last clause
         ;; add the error clause
         (append (parse-normal-clause last-clause key-fn key-test)
           (list `(error "There were no matches in the ECASE to value" ,key-test)))))))

(defmacro case (keyform &rest clauses)
  (declare (system::%java-class-name "lisp.common.function.Case"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
      ,(handle-normal-clauses clauses 'eql test-val))))

(defmacro ecase (keyform &rest clauses)
  (declare (system::%java-class-name "lisp.common.function.Ecase"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
      ,(handle-error-clauses clauses 'eql test-val nil))))

(defmacro typecase (keyform &rest clauses)
  (declare (system::%java-class-name "lisp.common.function.Typecase"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
       ,(handle-normal-clauses clauses 'typep test-val))))

(defmacro etypecase (keyform &rest clauses)
  (declare (system::%java-class-name "lisp.common.function.Etypecase"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
       ,(handle-error-clauses clauses 'typep test-val t))))
|#
;; Multiple-Value-Bind
#|
(defmacro multiple-value-bind ((&rest vars) value-form &body body)
  (declare (system::%java-class-name "jcl.compiler.functions.MultipleValueBind"))
  (let ((ignore (gensym)))
    `(multiple-value-call #'(lambda (&optional ,@vars &rest ,ignore)
                              (declare (ignore ,ignore))
                              ,@(if body body (list nil)))
       ,value-form)))
|#
;; Multiple-Value-List

(defmacro multiple-value-list (form)
  (declare (system::%java-class-name "jcl.compiler.functions.MultipleValueList"))
  `(multiple-value-call #'list ,form))

;; Multiple-Value-Setq
#|
(defmacro multiple-value-setq ((&rest vars) form)
  (declare (system::%java-class-name "lisp.common.function.MultipleValueSetq"))
  (let* ((ctr 0)
         (var-forms (mapcar #'(lambda (var) `(setq ,var (nth 0 xx-00))) vars)))
    (dolist (form-x var-forms)
      (rplaca (cdr (third form-x)) ctr)
      (setq ctr (1+ ctr)))
    `(let ((xx-00 (multiple-value-list ,form)))
       (progn ,@var-forms))))

(defmacro multiple-value-setq (varlist value-form)
  (declare (system::%java-class-name "jcl.compiler.functions.MultipleValueSetq"))
  (if varlist
      `(values (setf (values ,@varlist) ,value-form))
    `(values ,value-form)))
|#
;; Nth-Value

(defmacro nth-value (index form)
  (declare (system::%java-class-name "jcl.compiler.functions.NthValue"))
  `(nth ,index (multiple-value-list ,form)))

;; Prog,Prog*,Prog1,Prog2
#|
(defun parse-decls-body (decls-body)
  (let ((first (car decls-body)))
    (if (and (listp first)
             (eq (car first) 'declare))
        (multiple-value-bind (decls body)
                             (parse-decls-body (cdr decls-body))
          (values (cons first decls) body))
      (values nil decls-body))))

(defmacro prog (varlist &body decls-body)
  (declare (system::%java-class-name "jcl.compiler.functions.Prog"))
  (multiple-value-bind (decls body)
                       (parse-decls-body decls-body)
    `(block nil
       (let ,varlist
         ,@decls
         (tagbody ,@body)))))

(defmacro prog* (varlist &body decls-body)
  (declare (system::%java-class-name "jcl.compiler.functions.ProgStar"))
  (multiple-value-bind (decls body)
                       (parse-decls-body decls-body)
    `(block nil
       (let* ,varlist
         ,@decls
         (tagbody ,@body)))))
|#
(defmacro prog1 (first-form &rest forms)
  (declare (system::%java-class-name "jcl.compiler.functions.Prog1"))
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

(defmacro prog2 (first-form second-form &rest forms)
  (declare (system::%java-class-name "jcl.compiler.functions.Prog2"))
  (let ((result (gensym)))
    `(let ((,result (progn ,first-form ,second-form)))
       ,@forms
       ,result)))

;; Define-Modify-Macro

;; Defsetf

;; Define-Setf-Expander

;; Setf,PSetf

;; Shiftf

;; Rotatef

;;;;;;;;;;;;;;;;;;;;;;

(export '(return when unless cond and or multiple-value-bind multiple-value-list multiple-value-setq nth-value
		  prog prog* prog1 prog2)
        "COMMON-LISP")