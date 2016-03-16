;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
#|
(in-package "SYSTEM")

;;;;;;;;;;;;;;;;;;;;;;

(set-symbol-function
  'declarationp
  (common-lisp::lambda (form)
    (common-lisp::if (common-lisp::listp form)
        (common-lisp::eq (common-lisp::first form) 'declare))))

(set-symbol-function
  'trim-body
  (common-lisp::lambda (body)
    (common-lisp::if (common-lisp::null body)
        nil
      (common-lisp::if (common-lisp::null (common-lisp::rest body))
          body
        (common-lisp::if (common-lisp::stringp (common-lisp::first body))
            (trim-body (common-lisp::rest body))
          (common-lisp::if (declarationp (common-lisp::first body))
              (trim-body (common-lisp::rest body))
            body))))))

(set-symbol-function
  'trim-doc-and-decls
  (common-lisp::lambda (full-body body)
    (common-lisp::if (common-lisp::not (common-lisp::eq full-body body))
        (common-lisp::cons (common-lisp::first full-body)
              (trim-doc-and-decls (common-lisp::rest full-body) body)))))

;;;;;;;;;;;;;;;;;;;;;;

(common-lisp::export '(trim-body trim-doc-and-decls)
        "SYSTEM")

(common-lisp::in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(macro-lambda defmacro (name lambda-list &body doc-decls-body)
  (declare (system::%java-class-name "jcl.compiler.functions.Defmacro"))
  (let* ((body (system::trim-body doc-decls-body))
         (doc-decls (system::trim-doc-and-decls doc-decls-body body)))
    `(macro-lambda ,name ,lambda-list
      ,@doc-decls
      (block ,fn-name ,@body))))

(macro-lambda defun (name lambda-list &body doc-decls-body)
  (declare (system::%java-class-name "jcl.compiler.functions.Defun"))
  (let* ((body (system::trim-body doc-decls-body))
         (doc-decls (system::trim-doc-and-decls doc-decls-body body)))
    `(progn
      (system::set-symbol-function
        (quote ,name)
        (lambda ,lambda-list
          (declare (system::%lisp-name ,fn-name)) ,@doc-decls
          (block ,fn-name ,@body)))
      (quote ,name))))
|#
;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP")

(macro-lambda defmacro (name lambda-list &rest doc-decls-body)
  (declare (system:%java-class-name "jcl.compiler.functions.Defmacro"))
  `(macro-lambda ,name ,lambda-list ,@doc-decls-body))

(macro-lambda defun (name lambda-list &rest doc-decls-body)
  (declare (system:%java-class-name "jcl.compiler.functions.Defun"))
  `(progn
     (system::set-symbol-function
        (quote ,name)
        (lambda ,lambda-list ,@doc-decls-body))
     (quote ,name)))

;;;;;;;;;;;;;;;;;;;;;;

(export '(defmacro defun)
        "COMMON-LISP")