;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(in-package "COMMON-LISP")

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
              (if (not (if (consp form) (symbolp (car form))))
                  (return-from parse-body (values body decls doc))
                (if (eq (car form) 'declare)
                    (progn
                      (setq decls (cons form decls)
                            body (cdr body)
                            form (car body))
                      (go top))
                  (return-from parse-body (values body decls doc)))))))))))

(macro-lambda defmacro (name lambda-list &body doc-decls-body)
  (declare (system::%java-class-name "jcl.compiler.functions.Defmacro"))
  (let* ((parsed-body (multiple-value-call #'list (parse-body doc-decls-body)))
         (body (car parsed-body))
         (decls (car (cdr parsed-body)))
         (doc (car (cdr (cdr parsed-body)))))
    `(macro-lambda ,name ,lambda-list
      ,@decls
      ,doc
      (block ,name ,@body))))

(macro-lambda defun (name lambda-list &body doc-decls-body)
  (declare (system::%java-class-name "jcl.compiler.functions.Defun"))
  (let* ((parsed-body (multiple-value-call #'list (parse-body doc-decls-body)))
         (body (car parsed-body))
         (decls (car (cdr parsed-body)))
         (doc (car (cdr (cdr parsed-body)))))
    `(progn
      (system::set-symbol-function
        (quote ,name)
        (lambda ,lambda-list
          (declare (system::%lisp-name ,name))
          ,@decls
          ,doc
          (block ,name ,@body)))
      (quote ,name))))

;;;;;;;;;;;;;;;;;;;;;;

(export '(defmacro defun)
        "COMMON-LISP")