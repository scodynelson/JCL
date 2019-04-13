;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
#|
(eval-when (:load-toplevel :execute)
(export '()
        (find-package "COMMON-LISP"))
) ;eval-when
|#
(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Flush this out more??
(defun disassemble (fn)
  "Disassembles function designators or lambda expressions into symbolic instructions or expressions."
  (declare (system::%java-class-name "jcl.environment.functions.Disassemble"))
  (ext:jinvoke-static
    (ext:jmethod "disassemble" (ext:jclass "jcl.compiler.ClassFileUtils")
                 (ext:jclass "jcl.lang.FunctionStruct"))
    (ext:jinvoke-static
        (ext:jmethod "toLispFunction" (ext:jclass "jcl.lang.FunctionStruct")
                     (ext:jclass "jcl.lang.LispStruct"))
        fn)))

;;;;;;;;;;;;;;;;;;;;;;

(export '(disassemble)
        "COMMON-LISP")