;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;
#|
(defun constantly (x)
  (declare (system::%java-class-name "jcl.common.functions.Constantly"))
  #'(lambda (&rest args) (declare (ignore args)) x))
|#
;;;;;;;;;;;;;;;;;;;;;;

;; Not

(defun not (object)
  "Returns T if x is false; otherwise, returns NIL."
  (declare (system::%java-class-name "jcl.common.functions.Not"))
  (null object))

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
  (declare (system::%java-class-name "jcl.common.macros.Declaim"))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (proclaim ',declaration-specifier)))
|#

;; Destructuring-Bind

;; Return

(defmacro return (&optional result)
  (declare (system::%java-class-name "jcl.common.macros.Return"))
  `(return-from nil ,result))

;; When, Unless

(defmacro when (test-form &rest body)
  (declare (system::%java-class-name "jcl.common.macros.When"))
  (if (cdr body)
      `(if ,test-form (progn ,@body))
    `(if ,test-form ,(car body))))

(defmacro unless (test-form &rest body)
  (declare (system::%java-class-name "jcl.common.macros.Unless"))
  (if (cdr body)
      `(if (not ,test-form) (progn ,@body))
    `(if (not ,test-form) ,(car body))))

;; Defconstant, Defparameter, Devvar

(defmacro defvar (var &optional (val nil valp) (doc nil docp))
  (declare (system::%java-class-name "jcl.common.macros.Defvar"))
  `(progn
     ;(declaim (special ,var))
     ;(system::%set-special ',var t)
     ;,@(when valp ;; TODO: this when block should be used... but this is a hack
     ;    `((unless (boundp ',var)
     ;        (setq ,var ,val))))
     (setq ,var ,val)
     ;,@(when docp
     ;    `((setf (documentation ',var 'variable) ',doc)))
     ',var))

(defmacro defparameter (var val &optional (doc nil docp))
  (declare (system::%java-class-name "jcl.common.macros.Defparameter"))
  `(progn
     ;(declaim (special ,var))
     ;(system::%set-special ',symbol t)
     (setq ,var ,val)
     ;,@(when docp
     ;    `((setf (documentation ',var 'variable) ',doc)))
     ',var))

(defmacro defconstant (var val &optional (doc nil docp))
  (declare (system::%java-class-name "jcl.common.macros.Defconstant"))
  `(progn
     ;(declaim (special ,var))
     ;(system::%set-special ',symbol t)
     ;(system::%set-constant ',symbol nil)
     (setq ,var ,val)
     ;,@(when docp
     ;    `((setf (documentation ',var 'variable) ',doc)))
     ;(system::%set-constant ',symbol t)
     ',var))

;; Cond

(defmacro cond (&rest clauses)
  (declare (system::%java-class-name "jcl.common.macros.Cond"))
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

;; Psetq

(defun psetq-bindings (pairs)
  (when pairs
    (cons `(,(gensym) ,(car (cdr pairs)))
          (psetq-bindings (cdr (cdr pairs))))))

(defun map-psetq-pairs (pairs bindings)
  (when pairs
    (cons (car pairs)
          (cons (car (car bindings))
                (map-psetq-pairs (cdr (cdr pairs)) (cdr bindings))))))

(defmacro psetq (&whole w &rest pairs)
  (declare (system::%java-class-name "jcl.common.macros.Psetq"))
  (when pairs
    (unless (evenp (length pairs))
      (error "Uneven number of args in the call: ~S" w))
    (let ((bindings (psetq-bindings pairs)))
        `(progn
           (let ,bindings
             (setq ,@(map-psetq-pairs pairs bindings))))
           nil)))

;; And, Or

(defmacro and (&rest forms)
  (declare (system::%java-class-name "jcl.common.macros.And"))
  (cond ((null forms) t)
        ((null (cdr forms)) (car forms))
        (t
         `(if ,(car forms)
              (and ,@(cdr forms))
            nil))))

(defmacro or (&rest forms)
  (declare (system::%java-class-name "jcl.common.macros.Or"))
  (cond ((null forms) nil)
        ((null (cdr forms)) (car forms))
        (t
         (let ((result (gensym)))
           `(let ((,result ,(car forms)))
              (if ,result
                  ,result
                (or ,@(cdr forms))))))))

;; Case, Ccase, Ecase || Typecase, Ctypecase, Etypecase

(defun parse-normal-clause (clause key-fn key-test)
  (when clause
    (if (not (listp clause))
        (error "CASE error: clause must be a list, ~S" clause)
      (let ((key-form (car clause))
            (forms (cdr clause)))
        (when key-form ;; if the key-form is NIL, drop the entry
          (unless (listp key-form)
            (setq key-form (list key-form)))
          `(if (or ,@(mapcar #'(lambda (key) `(,key-fn ,key-test ',key)) key-form))
               (progn ,@forms)))))))

(defun handle-most-clauses (clauses key-fn key-test last-clause-callback)
    (let ((key-form (car (car clauses))))
      (when (and (cdr clauses) (or (eq key-form t) (eq key-form 'otherwise)))
        (error "T and OTHERWISE are not permitted in other than the last clause"))
      ;; handle the clauses
      (let ((pnc (parse-normal-clause (car clauses) key-fn key-test)))
        (if pnc
          (if (cdr clauses)
            (append pnc (list (handle-most-clauses (cdr clauses) key-fn key-test last-clause-callback)))
            (funcall last-clause-callback (car clauses)))
          (handle-most-clauses (cdr clauses) key-fn key-test last-clause-callback)))))

(defun handle-normal-clauses (clauses key-fn key-test)
  (handle-most-clauses clauses key-fn key-test
    ;; this is the last clause processor
    #'(lambda (last-clause)
       (let* ((key-form (car last-clause)))
         (if (or (eq key-form t)
                 (eq key-form 'otherwise))
           `(progn ,(car (cdr last-clause)))
           (parse-normal-clause last-clause key-fn key-test))))))

(defun handle-error-clauses (clauses key-fn key-test typecase-p)
  (handle-most-clauses clauses key-fn key-test
    ;; this is the last clause processor
    #'(lambda (last-clause)
       (let* ((key-form (car last-clause)))
         (when (and (or (eq key-form t)
                        (eq key-form 'otherwise))
                    (not typecase-p))
           (error "T and OTHERWISE are not permitted in ECASE"))
         ;; handle the last clause
         ;; add the error clause
         (append (parse-normal-clause last-clause key-fn key-test)
                 (list `(error "There were no matches in the ECASE to value" ,key-test)))))))

(defmacro case (keyform &rest clauses)
  (declare (system::%java-class-name "jcl.common.macros.Case"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
      ,(handle-normal-clauses clauses 'eql test-val))))

(defmacro ecase (keyform &rest clauses)
  (declare (system::%java-class-name "jcl.common.macros.Ecase"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
      ,(handle-error-clauses clauses 'eql test-val nil))))

(defmacro typecase (keyform &rest clauses)
  (declare (system::%java-class-name "jcl.common.macros.Typecase"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
       ,(handle-normal-clauses clauses 'typep test-val))))

(defmacro etypecase (keyform &rest clauses)
  (declare (system::%java-class-name "jcl.common.macros.Etypecase"))
  (let ((test-val (gensym)))
    `(let ((,test-val ,keyform))
       ,(handle-error-clauses clauses 'typep test-val t))))

;; Multiple-Value-Bind

(defmacro multiple-value-bind ((&rest vars) value-form &body body)
  (declare (system::%java-class-name "jcl.common.macros.MultipleValueBind"))
  (let ((ignore (gensym)))
    `(multiple-value-call #'(lambda (&optional ,@vars &rest ,ignore)
                              (declare (ignore ,ignore))
                              ,@(if body body (list nil)))
       ,value-form)))

;; Multiple-Value-List

(defmacro multiple-value-list (form)
  (declare (system::%java-class-name "jcl.common.macros.MultipleValueList"))
  `(multiple-value-call #'list ,form))

;; Multiple-Value-Setq
#|
(defmacro multiple-value-setq (varlist value-form)
  (declare (system::%java-class-name "jcl.common.macros.MultipleValueSetq"))
  (unless (and (listp varlist) (every #'symbolp varlist))
    (error "~S is not a list of symbols." varlist))
  (if varlist
      `(values (setf (values ,@varlist) ,value-form))
    `(values ,value-form)))
|#
;; Nth-Value

(defmacro nth-value (index form)
  (declare (system::%java-class-name "jcl.common.macros.NthValue"))
  `(nth ,index (multiple-value-list ,form)))

;; Prog,Prog*,Prog1,Prog2

(defmacro prog (varlist &body decls-body)
  (declare (system::%java-class-name "jcl.common.macros.Prog"))
  (multiple-value-bind (body decls)
                       (parse-body decls-body)
    `(block nil
       (let ,varlist
         ,@decls
         (tagbody ,@body)))))

(defmacro prog* (varlist &body decls-body)
  (declare (system::%java-class-name "jcl.common.macros.ProgStar"))
  (multiple-value-bind (body decls)
                       (parse-body decls-body)
    `(block nil
       (let* ,varlist
         ,@decls
         (tagbody ,@body)))))

(defmacro prog1 (first-form &rest forms)
  (declare (system::%java-class-name "jcl.common.macros.Prog1"))
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

(defmacro prog2 (first-form second-form &rest forms)
  (declare (system::%java-class-name "jcl.common.macros.Prog2"))
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

;; Assert

(defmacro assert (test-form &optional places datum &rest arguments)
  "Signals an error if the value of test-form is nil.  Continuing from this
   error using the CONTINUE restart will allow the user to alter the value of
   some locations known to SETF, starting over with test-form.  Returns nil."
  (declare (system::%java-class-name "jcl.common.macros.Assert"))
  `(let ((result (eval ,test-form)))
    (unless result
      (error "The assertion ~S failed." test-form))))

;;;;;;;;;;;;;;;;;;;;;;

(provide "macros")