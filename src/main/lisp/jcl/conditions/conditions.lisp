;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
  (require "iterators")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  (declare (system::%java-class-name "jcl.conditions.functions.HandlerBind"))
  (dolist (binding bindings)
    (unless (and (consp binding) (= (length binding) 2))
      (error "ill-formed handler binding ~S" binding)))
  `(let ((*handler-clusters*
          (cons (list ,@(mapcar (lambda (x) `(cons ',(car x) ,(cadr x)))
                                bindings))
                *handler-clusters*)))
     (multiple-value-prog1
      (progn ,@forms))))

(defmacro handler-case (form &rest cases)
  (declare (system::%java-class-name "jcl.conditions.functions.HandlerCase"))
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
        (let ((normal-return (make-symbol "normal-return"))
              (error-return  (make-symbol "error-return")))
          `(block ,error-return
             (multiple-value-call (lambda ,@(cdr no-error-clause))
                                  (block ,normal-return
                                    (return-from ,error-return
                                                 (handler-case (return-from ,normal-return ,form)
                                                   ,@(remove no-error-clause cases)))))))
        (let ((tag (gensym))
              (var (gensym))
              (annotated-cases (mapcar (lambda (case) (cons (gensym) case))
                                       cases)))
          `(block ,tag
             (let ((,var nil))
               (declare (ignorable ,var))
               (tagbody
                (handler-bind
                  ,(mapcar (lambda (annotated-case)
                             (list (cadr annotated-case)
                                   `(lambda (temp)
                                      ,(if (caddr annotated-case)
                                           `(setq ,var temp)
                                           '(declare (ignore temp)))
                                      (go ,(car annotated-case)))))
                           annotated-cases)
                  (return-from ,tag
                               ,form))
                ,@(mapcan
                   (lambda (annotated-case)
                     (list (car annotated-case)
                           (let ((body (cdddr annotated-case)))
                             `(return-from
                               ,tag
                               ,(cond ((caddr annotated-case)
                                       `(let ((,(caaddr annotated-case)
                                                ,var))
                                          ,@body))
                                      (t
                                       `(locally ,@body)))))))
                   annotated-cases))))))))

;;;;;;;;;;;;;;;;;;;;;;

(provide "conditions")