;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defun mapc (fn &rest lists)
  ""
  (declare (system::%java-class-name "jcl.lists.functions.MapC"))
  (ext:jinvoke-static
    (ext:jmethod "mapC" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    fn lists))

(defun mapcar (fn &rest lists)
  ""
  (declare (system::%java-class-name "jcl.lists.functions.MapCar"))
  (ext:jinvoke-static
    (ext:jmethod "mapCar" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    fn lists))

(defun mapcan (fn &rest lists)
  ""
  (declare (system::%java-class-name "jcl.lists.functions.MapCan"))
  (ext:jinvoke-static
    (ext:jmethod "mapCan" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    fn lists))

(defun mapl (fn &rest lists)
  ""
  (declare (system::%java-class-name "jcl.lists.functions.MapL"))
  (ext:jinvoke-static
    (ext:jmethod "mapL" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    fn lists))

(defun maplist (fn &rest lists)
  ""
  (declare (system::%java-class-name "jcl.lists.functions.MapList"))
  (ext:jinvoke-static
    (ext:jmethod "mapList" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    fn lists))

(defun mapcon (fn &rest lists)
  ""
  (declare (system::%java-class-name "jcl.lists.functions.MapCon"))
  (ext:jinvoke-static
    (ext:jmethod "mapCon" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    fn lists))

;;;;;;;;;;;;;;;;;;;;;;

(export '(mapc mapcar mapcan mapl maplist mapcon)
        "COMMON-LISP")

(provide "base-lists")