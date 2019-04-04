;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
#|
(eval-when (:load-toplevel :execute)
(export '()
        (find-package "COMMON-LISP"))
) ;eval-when
|#
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
  ($getLispName symbol))

(defun symbol-package (symbol)
  "Gets the package of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolPackage"))
  ($getPackage symbol))

(defun symbol-function (symbol)
  "Gets the function value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolFunction"))
  ($getFunction symbol))

;; TODO: setf-symbol-function

(defun symbol-plist (symbol)
  "Gets the plist value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolPlist"))
  ($getProperties symbol))

;; TODO: setf-symbol-plist

(defun symbol-value (symbol)
  "Gets the value of the provided symbol."
  (declare (system::%java-class-name "jcl.symbols.functions.SymbolValue"))
  ($getValue symbol))

;; TODO: setf-symbol-value

;;;;;;;;;;;;;;;;;;;;;;

(defun get (symbol indicator &optional default)
  "Finds a property on the property list of symbol whose property indicator is identical to indicator, and returns its corresponding property value."
  (declare (system::%java-class-name "jcl.symbols.functions.Get"))
  ($getProperty symbol indicator default))

(defun (setf get) (symbol indicator value &optional default)
  "Finds a property on the property list of symbol whose property indicator is identical to indicator, and sets its corresponding property value with the new-value provided."
  (declare (system::%java-class-name "jcl.hashtables.functions.SetfGet")
           (ignore default))
  ($setProperty symbol indicator value)
  value)

(defun remprop (symbol indicator)
  "Removes from the property list of symbol a property[1] with a property indicator identical to indicator."
  (declare (system::%java-class-name "jcl.symbols.functions.RemProp"))
  ($removeProperty symbol indicator))

;;;;;;;;;;;;;;;;;;;;;;

(defun boundp (symbol)
  "Returns true if symbol is bound; otherwise, returns false."
  (declare (system::%java-class-name "jcl.symbols.functions.BoundP"))
  ($hasValue symbol))

(defun makunbound (symbol)
  "Makes the symbol be unbound, regardless of whether it was previously bound."
  (declare (system::%java-class-name "jcl.symbols.functions.MakUnbound"))
  ($makunbound symbol))

(defun set (symbol value)
  "Sets the value of the provided symbol to the provided value."
  (declare (system::%java-class-name "jcl.symbols.functions.Set"))
  ($setValue symbol value)
  value)

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