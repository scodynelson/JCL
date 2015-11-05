;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

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
#|
(defun fill-out-var (var)
  (unless (listp var)
    (setq var (list var)))
  `(,(first var) ,(second var) ,@(when (third var) (list (third var)))))

(defun fill-out-vars (vars)
  (when vars
    (cons (fill-out-var (car vars)) (fill-out-vars (rest vars)))))

(defun initialize-vars (vars)
  (when vars
    (let ((first-var (first vars)))
      (cons `(,(first first-var) ,(second first-var)) (initialize-vars (rest vars))))))

(defun update-vars (vars)
  (when vars
    (if (cddar vars)
        (cons `(,(caar vars) ,(caddar vars)) (update-vars (rest vars)))
      (update-vars (rest vars)))))

(defun end-test-form (test-and-results)
  (first test-and-results))

(defun result-forms (test-and-results)
  (rest test-and-results))

(defun count-decl-forms (the-listof-stmts &optional (count 0))
  (if (eq (caar the-listof-stmts) 'declare)
      (count-decl-forms (rest the-listof-stmts) (1+ count))
    count))

(defun parse-decls-and-statements (decls-and-statements)
  (let ((count-of-decls (count-decl-forms decls-and-statements)))
    `(,(copy-first-n decls-and-statements count-of-decls) ,(nthcdr count-of-decls decls-and-statements))))

(defun count-decl-forms (the-listof-stmts &optional (count 0))
  (if (eq (caar the-listof-stmts) 'declare)
      (count-decl-forms (rest the-listof-stmts) (1+ count))
    count))

(defun copy-first-n (list count)
 (when (plusp count)
   (cons (first list) (copy-first-n (rest list) (1- count)))))

(defun flatten-pairs (pairs)
  (when pairs
    (cons (caar pairs) (cons (cadar pairs) (flatten-pairs (rest pairs))))))

(defmacro do (vars (end-test-form &rest result-forms) &rest decls-and-statements)
  (declare (system::%java-class-name "lisp.common.function.Do"))
  (let* ((full-vars (fill-out-vars vars))
         (init-vars (initialize-vars full-vars))
         (update-vars (update-vars full-vars))
         (decls-stmts (parse-decls-and-statements decls-and-statements))
         (decls (first decls-stmts))
         (stmts (second decls-stmts))
         (top-label (gensym "TopLabel-"))
         (end-label (gensym "EndLabel-")))
    `(block nil
       (let ,init-vars
         (declare ,@decls)
         (tagbody
           ,top-label
           (if ,end-test-form
               (go ,end-label)
             (progn
               ,@stmts
               (psetq ,@(flatten-pairs update-vars))
               (go ,top-label)))
           ,end-label)
           ,@result-forms))))

(defmacro do* (vars (end-test-form &rest result-forms) &rest decls-and-statements)
  (declare (system::%java-class-name "lisp.common.function.DoStar"))
  (let* ((full-vars (fill-out-vars vars))
         (init-vars (initialize-vars full-vars))
         (update-vars (update-vars full-vars))
         (decls-stmts (parse-decls-and-statements decls-and-statements))
         (decls (first decls-stmts))
         (stmts (second decls-stmts))
         (top-label (gensym "TopLabel-"))
         (end-label (gensym "EndLabel-")))
    `(block nil
       (let* ,init-vars
         (declare ,@decls)
         (tagbody
           ,top-label
           (if ,end-test-form
               (go ,end-label)
             (progn
               ,@stmts
               (setq ,@(flatten-pairs update-vars))
               (go ,top-label)))
           ,end-label)
           ,@result-forms))))
|#

;ABCL
#|
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
|#
;;;;;;;;;;;;;;;;;;;;;;

(export '(dolist dotimes do do*)
        "COMMON-LISP")