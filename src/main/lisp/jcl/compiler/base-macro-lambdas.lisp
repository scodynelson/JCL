;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(macro-lambda defmacro (name lambda-list &rest doc-decls-body)
  (declare (system:%java-class-name "jcl.compiler.function.Defmacro"))
  `(macro-lambda ,name ,lambda-list ,@doc-decls-body))

(macro-lambda defun (name lambda-list &rest doc-decls-body)
  (declare (system:%java-class-name "jcl.compiler.function.Defun"))
  `(progn
     (system::set-symbol-function
        (quote ,name)
        (lambda ,lambda-list ,@doc-decls-body))
     (quote ,name)))

;;;;;;;;;;;;;;;;;;;;;;

(export '(defmacro defun)
        "COMMON-LISP")