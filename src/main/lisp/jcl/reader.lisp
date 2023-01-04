;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "streams")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: def-var: *read-base* *read-default-float-format* *read-eval* *read-suppress* *readtable*
#|
(defvar *read-eval* t
  "If false, then the #. read macro is disabled.")

(defvar *read-default-float-format* 'single-float "Float format for 1.0E1")
(declaim (type (member short-float single-float double-float long-float)
               *read-default-float-format*))

(defvar *read-suppress* nil
  "Suppresses most interpreting of the reader when T")

(defvar *read-base* 10
  "The radix that Lisp reads numbers in.")
(declaim (type (integer 2 36) *read-base*))

(defvar *readtable*)
(declaim (type readtable *readtable*))
(setf (documentation '*readtable* 'variable)
      "Variable bound to current readtable.")
|#
;;;;;;;;;;;;;;;;;;;;;;

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "Copies from-readtable."
  (declare (system::%java-class-name "jcl.reader.functions.CopyReadtable"))
  (ext:jinvoke-interface
    (ext:jmethod "copyReadtable" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    from-readtable to-readtable))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Review the 'system::read-dispatch-character' bit
(defun make-dispatch-macro-character (char &optional non-terminating-p (readtable *readtable*))
  "Makes char be a dispatching macro character in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.MakeDispatchMacroCharacter"))
  (ext:jinvoke-interface
    (ext:jmethod "makeDispatchMacroCharacter" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.BooleanStruct"))
    readtable (symbol-function 'system::read-dispatch-character) char non-terminating-p))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: read/read-preserving-whitespace

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: read-delimited-list

;;;;;;;;;;;;;;;;;;;;;;

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace)
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned.  Macro chars
   will take effect."
  (declare (system::%java-class-name "jcl.reader.functions.ReadFromString"))
  (let ((stream (make-string-input-stream string start end)))
    (if preserve-whitespace
        (read-preservice-whitespace stream eof-error-p eof-value nil)
      (read stream eof-error-p eof-value nil))))

;;;;;;;;;;;;;;;;;;;;;;

(defun readtable-case (readtable)
  "Gets the readtable case of the provided readtable."
  (declare (system::%java-class-name "jcl.reader.functions.ReadtableCase"))
  (ext:jinvoke-interface
    (ext:jmethod "readtableCase" (ext:jclass "jcl.lang.ReadtableStruct"))
    readtable))

;; TODO: setf-readtable-case

;;;;;;;;;;;;;;;;;;;;;;

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  "Retrieves the dispatch function associated with disp-char and sub-char in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.GetDispatchMacroCharacter"))
  (ext:jinvoke-interface
    (ext:jmethod "getDispatchMacroCharacter" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.CharacterStruct"))
    readtable disp-char sub-char))

(defun set-dispatch-macro-character (disp-char sub-char new-function &optional (readtable *readtable*))
  "Causes new-function to be called when disp-char followed by sub-char is read."
  (declare (system::%java-class-name "jcl.reader.functions.SetDispatchMacroCharacter"))
  (ext:jinvoke-interface
    (ext:jmethod "setDispatchMacroCharacter" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.FunctionStruct"))
    readtable disp-char sub-char new-function))

;;;;;;;;;;;;;;;;;;;;;;

(defun get-macro-character (char &optional (readtable *readtable*))
  "Returns the reader macro function associated with char in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.GetMacroCharacter"))
  (ext:jinvoke-interface
    (ext:jmethod "getMacroCharacter" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.CharacterStruct"))
    readtable char))

(defun set-macro-character (char new-function &optional non-terminating-p (readtable *readtable*))
  "Causes char to be a macro character associated with the reader macro function new-function in readtable."
  (declare (system::%java-class-name "jcl.reader.functions.SetMacroCharacter"))
  (ext:jinvoke-interface
    (ext:jmethod "setMacroCharacter" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.FunctionStruct")
                 (ext:jclass "jcl.lang.BooleanStruct"))
    readtable char new-function non-terminating-p))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: set-syntax-from-char

;;;;;;;;;;;;;;;;;;;;;;

(provide "reader")