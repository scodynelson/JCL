;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (system::%in-package "COMMON-LISP")
) ;eval-when

;;;;;;;;;;;;;;;;;;;;;;

(system::set-symbol-function
  'parse-body
  (lambda (body &optional (doc-string-allowed t))
    (declare (system::%java-class-name "jcl.compiler.functions.ParseBody"))
    (block parse-body
      (let ((decls nil)
            (doc nil)
            (form (car body)))
        (tagbody
          top
          (if (null body)
              (values body decls doc)
            (if (if (stringp form) (cdr body))
                (if doc-string-allowed
                    (progn
                      (setq doc form
                            doc-string-allowed nil
                            body (cdr body)
                            form (car body))
                      (go top))
                  (return-from parse-body (values body decls doc)))
              (if (null (if (consp form) (symbolp (car form))))
                  (return-from parse-body (values body decls doc))
                (if (eq (car form) 'declare)
                    (progn
                      (setq decls (cons form decls)
                            body (cdr body)
                            form (car body))
                      (go top))
                  (return-from parse-body (values body decls doc)))))))))))

(system::set-symbol-function
  'defun-setf-p
  (lambda (function-name)
    (declare (system::%java-class-name "jcl.compiler.functions.DefunSetfP"))
    (if (symbolp function-name)
        nil
      (if (consp function-name)
          (if (eq (car function-name) 'setf)
              (if (consp (cdr function-name))
                  (if (symbolp (car (cdr function-name)))
                      (if (null (cdr (cdr function-name)))
                          t
                        (error "Too many elements in CONS name"))
                    (error "Second element in CONS name must be a SYMBOL"))
                (error "CONS name must be a proper list"))
            (error "CONS name must start with SYMBOL: SETF"))
        (error "Name must either be SYMBOL or CONS")))))

(system::set-symbol-function
  'fdefinition-block-name
  (lambda (function-name)
    (declare (system::%java-class-name "jcl.compiler.functions.FDefinitionBlockName"))
    (if (symbolp function-name)
        function-name
      (if (defun-setf-p function-name)
          (car (cdr function-name))
        (error "Name must either be SYMBOL or CONS")))))

(compiler::macro-lambda defmacro (name lambda-list &body doc-decls-body)
  (declare (system::%java-class-name "jcl.common.functions.Defmacro"))
  (let* ((parsed-body (multiple-value-call #'list (parse-body doc-decls-body)))
         (body (car parsed-body))
         (decls (car (cdr parsed-body)))
         (doc (car (cdr (cdr parsed-body)))))
    `(eval-when (:compile-toplevel :load-toplevel)
       (compiler::macro-lambda ,name ,lambda-list
         ,@decls
         ,doc
         (block ,name ,@body)))))

(compiler::macro-lambda defun (name lambda-list &body doc-decls-body)
  (declare (system::%java-class-name "jcl.common.functions.Defun"))
  (let* ((parsed-body (multiple-value-call #'list (parse-body doc-decls-body)))
         (body (car parsed-body))
         (decls (car (cdr parsed-body)))
         (doc (car (cdr (cdr parsed-body))))
         (block-name (fdefinition-block-name name))
         (setf-function-p (defun-setf-p name))
         (set-symbol-function-name (if setf-function-p
                                       'system::set-symbol-setf-function
                                       'system::set-symbol-function)))
    `(progn
      (,set-symbol-function-name
        (quote ,name)
        (lambda ,lambda-list
          (declare (system::%lisp-name ,name))
          ,@decls
          ,doc
          (block ,block-name ,@body)))
      (quote ,name))))

(compiler::macro-lambda in-package (name)
  (declare (system::%java-class-name "jcl.packages.functions.InPackage"))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (system::%in-package ,name)))

;;;;;;;;;;;;;;;;;;;;;;

(export '(defmacro defun in-package)
        "COMMON-LISP")

(provide "base-macro-lambdas")