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
  ($copySymbol symbol copy-properties))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: *gensym-counter*
;; TODO: gensym
;; TODO: gentemp

;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-name (symbol)
  "Gets the name of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolName"))
  ($symbolName symbol))

(defun symbol-package (symbol)
  "Gets the package of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolPackage"))
  ($symbolPackage symbol))

(defun symbol-function (symbol)
  "Gets the function value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolFunction"))
  ($symbolFunction symbol))

(defun (setf symbol-function) (symbol new-contents)
  "Sets the contents of symbol's function cell the new-contents provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfSymbolFunction"))
  ($setSymbolFunction symbol new-contents))

(defun symbol-plist (symbol)
  "Gets the plist value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolPlist"))
  ($symbolPlist symbol))

(defun (setf symbol-plist) (symbol new-plist)
  "Sets the contents of symbol's property-list cell the new-plist provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfSymbolPlist"))
  ($setSymbolPlist symbol new-plist))

(defun symbol-value (symbol)
  "Gets the value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolValue"))
  ($symbolValue symbol))

(defun (setf symbol-value) (symbol new-value)
  "Sets the contents of symbol's value cell the new-value provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfSymbolValue"))
  ($setSymbolValue symbol new-value))

;;;;;;;;;;;;;;;;;;;;;;

(defun get (symbol indicator &optional default)
  "Finds a property on the property list of symbol whose property indicator is identical to indicator, and returns its corresponding property value."
  (declare (system::%java-class-name "jcl.symbols.functions.Get"))
  ($getProp symbol indicator default))

(defun (setf get) (symbol indicator value &optional default)
  "Finds a property on the property list of symbol whose property indicator is identical to indicator, and sets its corresponding property value with the new-value provided."
  (declare (system::%java-class-name "jcl.symbols.functions.SetfGet")
           (ignore default))
  ($setProp symbol indicator value)
  value)

(defun remprop (symbol indicator)
  "Removes from the property list of symbol a property[1] with a property indicator identical to indicator."
  (declare (system::%java-class-name "jcl.symbols.functions.RemProp"))
  ($remProp symbol indicator))

;;;;;;;;;;;;;;;;;;;;;;

(defun boundp (symbol)
  "Returns true if symbol is bound; otherwise, returns false."
  (declare (system::%java-class-name "jcl.symbols.functions.BoundP"))
  ($boundP symbol))

(defun makunbound (symbol)
  "Makes the symbol be unbound, regardless of whether it was previously bound."
  (declare (system::%java-class-name "jcl.symbols.functions.MakUnbound"))
  ($makunbound symbol))

(defun set (symbol value)
  "Sets the value of the provided symbol to the provided value."
  (declare (system::%java-class-name "jcl.symbols.functions.Set"))
  ($setSymbolValue symbol value))

;;;;;;;;;;;;;;;;;;;;;;

(export '(make-symbol
          copy-symbol
          *gensym-counter*
          gensym
          gentemp
          symbol-name symbol-package symbol-function symbol-plist symbol-value
          get remprop
          boundp makunbound set)
        "COMMON-LISP")

(provide "symbols")