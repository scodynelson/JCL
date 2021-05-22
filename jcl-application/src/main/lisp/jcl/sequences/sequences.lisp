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
  ($elt sequence index))

(defun (setf elt) (sequence index value)
  "Sets the element of sequence specified by index to the new-value provided."
  (declare (system::%java-class-name "jcl.sequences.functions.SetfElt"))
  ($setfElt sequence value index))

(defun length (sequence)
  "Returns the number of elements in sequence."
  (declare (system::%java-class-name "jcl.sequences.functions.Length"))
  ($length sequence))

(defun reverse (sequence)
  "Returns a new sequence of the same kind as sequence, containing the same elements, but in reverse order."
  (declare (system::%java-class-name "jcl.sequences.functions.Reverse"))
  ($reverse sequence))

(defun nreverse (sequence)
  "Returns a new sequence of the same kind as sequence, containing the same elements, but in reverse order; the original sequence may be modified."
  (declare (system::%java-class-name "jcl.sequences.functions.NReverse"))
  ($nReverse sequence))

;;;;;;;;;;;;;;;;;;;;;;

(provide "sequences")