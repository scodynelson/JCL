;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: def-var: *read-base* *read-default-float-format* *read-eval* *read-suppress* *readtable*

;;;;;;;;;;;;;;;;;;;;;;

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "Copies from-readtable."
  (declare (system::%java-class-name "jcl.reader.functions.CopyReadtable"))
  ($copyReadtable from-readtable to-readtable))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Review the 'system::read-dispatch-character' bit
(defun make-dispatch-macro-character (char &optional non-terminating-p (readtable *readtable*))
  "Makes char be a dispatching macro character in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.MakeDispatchMacroCharacter"))
  ($makeDispatchMacroCharacter readtable (symbol-function 'system::read-dispatch-character) char non-terminating-p))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: read/read-preserving-whitespace

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: read-delimited-list

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: read-from-string

;;;;;;;;;;;;;;;;;;;;;;

(defun readtable-case (readtable)
  "Gets the readtable case of the provided readtable."
  (declare (system::%java-class-name "jcl.reader.functions.ReadtableCase"))
  ($readtableCase readtable))

;; TODO: setf-readtable-case

;;;;;;;;;;;;;;;;;;;;;;

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  "Retrieves the dispatch function associated with disp-char and sub-char in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.GetDispatchMacroCharacter"))
  ($getDispatchMacroCharacter readtable disp-char sub-char))

(defun set-dispatch-macro-character (disp-char sub-char new-function &optional (readtable *readtable*))
  "Causes new-function to be called when disp-char followed by sub-char is read."
  (declare (system::%java-class-name "jcl.reader.functions.SetDispatchMacroCharacter"))
  ($setDispatchMacroCharacter readtable disp-char sub-char new-function))

;;;;;;;;;;;;;;;;;;;;;;

(defun get-macro-character (char &optional (readtable *readtable*))
  "Returns the reader macro function associated with char in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.GetMacroCharacter"))
  ($getMacroCharacter readtable char))

(defun set-macro-character (char new-function &optional non-terminating-p (readtable *readtable*))
  "Causes char to be a macro character associated with the reader macro function new-function in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.SetMacroCharacter"))
  ($setMacroCharacter readtable char new-function non-terminating-p))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: set-syntax-from-char

;;;;;;;;;;;;;;;;;;;;;;

(provide "reader")