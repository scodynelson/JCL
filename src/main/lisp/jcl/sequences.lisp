;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Clean these up???

(defun elt (sequence index)
  "Accesses the element of sequence specified by index."
  (declare (system::%java-class-name "jcl.sequences.functions.Elt"))
  (ext:jinvoke-interface
    (ext:jmethod "elt" (ext:jclass "jcl.lang.SequenceStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    sequence index))

(defun length (sequence)
  "Returns the number of elements in sequence."
  (declare (system::%java-class-name "jcl.sequences.functions.Length"))
  (ext:jinvoke-interface
    (ext:jmethod "length" (ext:jclass "jcl.lang.SequenceStruct"))
    sequence))

(defun reverse (sequence)
  "Returns a new sequence of the same kind as sequence, containing the same elements, but in reverse order."
  (declare (system::%java-class-name "jcl.sequences.functions.Reverse"))
  (ext:jinvoke-interface
    (ext:jmethod "reverse" (ext:jclass "jcl.lang.SequenceStruct"))
    sequence))

(defun nreverse (sequence)
  "Returns a new sequence of the same kind as sequence, containing the same elements, but in reverse order; the original sequence may be modified."
  (declare (system::%java-class-name "jcl.sequences.functions.NReverse"))
  (ext:jinvoke-interface
    (ext:jmethod "nReverse" (ext:jclass "jcl.lang.SequenceStruct"))
    sequence))

;;;;;;;;;;;;;;;;;;;;;;

(provide "sequences")