;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "SYSTEM")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Clean these up??

(defun make-structure-instance (symbol)
  "Makes a new structure-object instance of the structure-class assigned to the provided symbol with the provided
  arguments as slot values."
  (declare (system::%java-class-name "jcl.structures.functions.MakeStructureInstance"))
  (ext:jinvoke-static
    (ext:jmethod "makeStructureInstance" (ext:jclass "jcl.lang.StructureObjectStruct")
                 (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun get-structure-slot-value (structure-class-symbol structure-instance slot-name)
  "Gets the slot value matching the provided symbol for the provided structure-object."
  (declare (system::%java-class-name "jcl.structures.functions.GetStructureSlotValue"))
  (ext:jinvoke-static
    (ext:jmethod "getStructureSlotValue" (ext:jclass "jcl.lang.StructureObjectStruct")
                 (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.StructureObjectStruct")
                 (ext:jclass "jcl.lang.SymbolStruct"))
    structure-class-symbol structure-instance slot-name))

(defun set-structure-slot-value (structure-class-symbol structure-instance slot-name slot-value)
  "Sets the slot value matching the provided symbol for the provided structure-object to the provided value."
  (declare (system::%java-class-name "jcl.structures.functions.SetStructureSlotValue"))
  (ext:jinvoke-static
    (ext:jmethod "setStructureSlotValue" (ext:jclass "jcl.lang.StructureObjectStruct")
                 (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.StructureObjectStruct")
                 (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    structure-class-symbol structure-instance slot-name slot-value))

;;;;;;;;;;;;;;;;;;;;;;

(provide "structures")