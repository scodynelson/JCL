;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; DOLIST
(defmacro dolist ((var list-form &optional result-form) &body body)
  (declare (system::%java-class-name "jcl.iterators.Dolist"))
  (multiple-value-bind (forms decls)
                       (parse-body body nil)
    (let ((list (gensym "LIST-"))
          (top (gensym "TOP-")))
      `(block nil
         (let ((,list ,list-form)
               (,var nil))
           ,@decls
           (tagbody
             ,top
             (when ,list
               (setq ,var (car ,list))
               (tagbody ,@forms)
               (setq ,list (cdr ,list))
               (go ,top)))
           ,result-form)))))

;; DOTIMES
(defmacro dotimes ((var count-form &optional result-form) &body body)
  (declare (system::%java-class-name "jcl.iterators.Dotimes"))
  (multiple-value-bind (forms decls)
                       (parse-body body nil)
    (let ((count (gensym "COUNT-"))
          (top (gensym "TOP-")))
      `(block nil
         (let ((,var 0)
               (,count ,count-form))
           ,@decls
           (tagbody
             ,top
             (when (< ,var ,count)
               (tagbody
                 ,@forms
                 (setq ,var (1+ ,var))
                 (go ,top))))
           ,result-form)))))

;; DO, DO*
(defun do-do-body (varlist endlist decls-and-code bind step name block)
  (let* ((inits ())
	     (steps ())
	     (L1 (gensym))
	     (L2 (gensym)))
    ;; Parse the varlist to get inits and steps.
    (dolist (v varlist)
      (cond ((symbolp v) (push v inits))
		    ((listp v)
		     (unless (symbolp (first v))
		       (error "~S step variable is not a symbol: ~S" name (first v)))
		     (case (length v)
		       (1 (push (first v) inits))
		       (2 (push v inits))
		       (3 (push (list (first v) (second v)) inits)
			      (setq steps (list* (third v) (first v) steps)))
		       (t (error "~S is an illegal form for a ~S varlist." v name))))
		    (t
		     (error "~S is an illegal form for a ~S varlist." v name))))
    ;; Construct the new form.
    (multiple-value-bind (code decls)
                         (parse-body decls-and-code nil)
      `(block ,block
         (,bind ,(nreverse inits)
          ,@decls
          (tagbody
           (go ,L2)
           ,L1
           ,@code
           (,step ,@(nreverse steps))
           ,L2
           (unless ,(car endlist)
             (go ,L1))
           (return-from ,block (progn ,@(cdr endlist)))))))))

(defmacro do (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let 'psetq 'do nil))

(defmacro do* (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let* 'setq 'do* nil))

;;;;;;;;;;;;;;;;;;;;;;

(provide "iterators")