;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defun make-symbol (name)
  "Creates and returns a fresh, uninterned symbol whose name is the given name."
  (declare (system::%java-class-name "jcl.symbols.functions.MakeSymbol"))
  (ext:jinvoke-static
    (ext:jmethod "toLispSymbol" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.StringStruct"))
    name))

;;;;;;;;;;;;;;;;;;;;;;

(defun copy-symbol (symbol &optional copy-properties)
  "Returns a fresh, uninterned symbol, the name of which is equal to and possibly the same as the name of the given symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.CopySymbol"))
  (ext:jinvoke-interface
    (ext:jmethod "copySymbol" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.BooleanStruct"))
    symbol copy-properties))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: *gensym-counter*
#|
(defvar *gensym-counter* 0
  "Counter for generating unique GENSYM symbols.")
(declaim (type unsigned-byte *gensym-counter*))
|#
;; TODO: gensym
;; TODO: gentemp

;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-name (symbol)
  "Gets the name of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolName"))
  (ext:jinvoke-interface
    (ext:jmethod "symbolName" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun symbol-package (symbol)
  "Gets the package of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolPackage"))
  (ext:jinvoke-interface
    (ext:jmethod "symbolPackage" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun symbol-function (symbol)
  "Gets the function value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolFunction"))
  (ext:jinvoke-interface
    (ext:jmethod "symbolFunction" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun (setf symbol-function) (symbol new-contents)
  "Sets the contents of symbol's function cell the new-contents provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfSymbolFunction"))
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolFunction" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.FunctionStruct"))
    symbol new-contents))

(defun symbol-plist (symbol)
  "Gets the plist value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolPlist"))
  (ext:jinvoke-interface
    (ext:jmethod "symbolPlist" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun (setf symbol-plist) (symbol new-plist)
  "Sets the contents of symbol's property-list cell the new-plist provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfSymbolPlist"))
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolPlist" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    symbol new-plist))

(defun symbol-value (symbol)
  "Gets the value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolValue"))
  (ext:jinvoke-interface
    (ext:jmethod "symbolValue" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun (setf symbol-value) (symbol new-value)
  "Sets the contents of symbol's value cell the new-value provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfSymbolValue"))
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol new-value))

;;;;;;;;;;;;;;;;;;;;;;

(defun get (symbol indicator &optional default)
  "Finds a property on the property list of symbol whose property indicator is identical to indicator, and returns its corresponding property value."
  (declare (system::%java-class-name "jcl.symbols.functions.Get"))
  (ext:jinvoke-interface
    (ext:jmethod "getProp" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol indicator default))

(defun (setf get) (symbol indicator value &optional default)
  "Finds a property on the property list of symbol whose property indicator is identical to indicator, and sets its corresponding property value with the new-value provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfGet")
           (ignore default))
  (ext:jinvoke-interface
    (ext:jmethod "setProp" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol indicator value))

(defun remprop (symbol indicator)
  "Removes from the property list of symbol a property[1] with a property indicator identical to indicator."
  (declare (system::%java-class-name "jcl.symbols.functions.RemProp"))
  (ext:jinvoke-interface
    (ext:jmethod "remProp" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol indicator))

;;;;;;;;;;;;;;;;;;;;;;

(defun boundp (symbol)
  "Returns true if symbol is bound; otherwise, returns false."
  (declare (system::%java-class-name "jcl.symbols.functions.BoundP"))
  (ext:jinvoke-interface
    (ext:jmethod "boundP" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun fboundp (name)
  "Returns true if name is fbound; otherwise, returns false."
  (declare (system::%java-class-name "jcl.symbols.functions.FBoundP"))
  (if (symbolp name)
      (ext:jinvoke-interface
        (ext:jmethod "fBoundP" (ext:jclass "jcl.lang.SymbolStruct"))
        name)
    (if (consp name)
        (get (cadr name) 'SETF-DEFINITION)
      (error "Name must either be SYMBOL or CONS"))))

(defun makunbound (symbol)
  "Makes the symbol be unbound, regardless of whether it was previously bound."
  (declare (system::%java-class-name "jcl.symbols.functions.MakUnbound"))
  (ext:jinvoke-interface
    (ext:jmethod "makunbound" (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

(defun set (symbol value)
  "Sets the value of the provided symbol to the provided value."
  (declare (system::%java-class-name "jcl.symbols.functions.Set"))
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol value))

;;;;;;;;;;;;;;;;;;;;;;

(provide "symbols")