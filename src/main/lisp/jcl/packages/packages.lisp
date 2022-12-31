;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
  (require "iterators")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTION ;;
;;;;;;;;;;;;;;;;;;;;;;

;;; validate-package-usability  --  Internal
;;;
;;;    Validate usability of the provided package.
;;;
(defun validate-package-usability (package)
  (unless (package-name package)
    (error "Can't do anything to a deleted package: ~S" thing))
  (when (eql package (find-package "KEYWORD"))
    (error "Can't use KEYWORD package: ~S" thing)))

;;; package-listify  --  Internal
;;;
;;;    Return a list of packages given a package-or-string-or-symbol or list thereof, checking types.
;;;
(defun package-listify (thing)
  (cond ((listp thing)
         (dolist (s thing)
           (let ((package (find-package s)))
             (if (packagep package)
                 (validate-package-usability package)
               (error "~S is not a package." s))))
         thing)
        ((packagep thing)
         (validate-package-usability thing)
         (list thing))
        (t
         (error "~S is neither a package nor a list of packages." thing))))

;;; Symbol-Listify  --  Internal
;;;
;;;    Take a symbol-or-list-of-symbols and return a list, checking types.
;;;
(defun symbol-listify (thing)
  (cond ((listp thing)
         (dolist (s thing)
           (unless (symbolp s)
             (error "~S is not a symbol." s)))
         thing)
        ((symbolp thing)
         (list thing))
        (t
         (error "~S is neither a symbol nor a list of symbols." thing))))

;;; String-Listify  --  Internal
;;;
;;;    Take a string-or-list-of-strings and return a list, checking types.
;;;
(defun string-listify (thing)
  (if (listp thing)
      (package-name-list thing)
    (list (string thing))))

;;; Package-Name-List  --  Internal
;;;
;;;    Take the list of names and ensures that the list is a list of Strings by checking that each name is a String
;;;         Designator, and coercing it into a String type.
;;;
(defun package-name-list (names)
  (if names
      (let ((names (reverse names))
            (new-names nil))
        (dolist (name names)
          (setq new-names (cons (string name) new-names)))
        (nreverse new-names))
    names))

;;;;;;;;;;;;;;;;;;;;;;

(defun find-package (name)
  "Locates and returns the package whose name or nickname is name."
  (declare (system::%java-class-name "jcl.packages.functions.FindPackage"))
  (ext:jinvoke-static
    (ext:jmethod "findPackage" (ext:jclass "jcl.lang.PackageStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    name))

(defun list-all-packages ()
  "Returns a fresh list of all registered packages."
  (declare (system::%java-class-name "jcl.packages.functions.ListAllPackages"))
  (ext:jinvoke-static (ext:jmethod "listAllPackages" (ext:jclass "jcl.lang.PackageStruct"))))

;;;;;;;;;;;;;;;;;;;;;;

(defun package-name (package)
  "Returns the string that names package."
  (declare (system::%java-class-name "jcl.packages.functions.PackageName"))
  (let* ((package (find-package package)))
    ($packageName package)))

(defun package-nicknames (package)
  "Returns the list of nickname strings for package, not including the name of package."
  (declare (system::%java-class-name "jcl.packages.functions.PackageNicknames"))
  (let ((package (find-package package)))
    ($packageNicknames package)))

(defun package-shadowing-symbols (package)
  "Returns a list of symbols that have been declared as shadowing symbols in package by shadow or shadowing-import."
  (declare (system::%java-class-name "jcl.packages.functions.PackageShadowingSymbols"))
  (let ((package (find-package package)))
    ($packageShadowingSymbols package)))

(defun package-use-list (package)
  "Returns a list of other packages that use package."
  (declare (system::%java-class-name "jcl.packages.functions.PackageUseList"))
  (let ((package (find-package package)))
    ($packageUseList package)))

(defun package-used-by-list (package)
  "Returns a list of other packages used by package."
  (declare (system::%java-class-name "jcl.packages.functions.PackageUsedByList"))
  (let ((package (find-package package)))
    ($packageUsedByList package)))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: packagep

;;;;;;;;;;;;;;;;;;;;;;

(defun find-symbol (symbol-name &optional (package *package*))
  "Locates a symbol whose name is symbol-name in a package."
  (declare (system::%java-class-name "jcl.packages.functions.FindSymbol"))
  (let ((package (find-package package))
        (symbol-name (string symbol-name)))
    ($toValues ($findSymbol package symbol-name))))

(defun find-all-symbols (symbol-name)
  "Searches every registered package for symbols that have a name that is the same as string."
  (declare (system::%java-class-name "jcl.packages.functions.FindAllSymbols"))
  (let ((symbol-name (string symbol-name)))
    ($findAllSymbols symbol-name)))

;;;;;;;;;;;;;;;;;;;;;;

(defun intern (symbol &optional (package *package*))
  "Enters a symbol named string into package."
  (declare (system::%java-class-name "jcl.packages.functions.Intern"))
  (let ((package (find-package package)))
    ($toValues ($intern package symbol))))

(defun unintern (symbol &optional (package *package*))
  "Removes symbol from package."
  (declare (system::%java-class-name "jcl.packages.functions.Unintern"))
  (let ((package (find-package package)))
    ($unintern package symbol)))

;;;;;;;;;;;;;;;;;;;;;;

(defun use-package (packages &optional (package *package*))
  "Causes package to inherit all the external symbols of packages-to-use."
  (declare (system::%java-class-name "jcl.packages.functions.UsePackage"))
  (let ((package (find-package package))
        (packages (package-listify symbols)))
    ($usePackage package packages)))

(defun unuse-package (packages &optional (package *package*))
  "Causes package to cease inheriting all the external symbols of packages-to-unuse."
  (declare (system::%java-class-name "jcl.packages.functions.UnusePackage"))
  (let ((package (find-package package))
        (packages (package-listify symbols)))
    ($unusePackage package packages)))

;;;;;;;;;;;;;;;;;;;;;;
#|
(defun export (symbols &optional (package *package*))
  "Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of
  that package."
  (declare (system::%java-class-name "jcl.packages.functions.Export"))
  (let ((package (find-package package))
        (symbols (symbol-listify symbols)))
    ($export package symbols)))
|#
(defun import (symbols &optional (package *package*))
  "Adds symbol or symbols to the internals of package, checking for name conflicts with existing symbols either present
  in package or accessible to it."
  (declare (system::%java-class-name "jcl.packages.functions.Import"))
  (let ((package (find-package package))
        (symbols (symbol-listify symbols)))
    ($importSymbols package symbols)))

;;;;;;;;;;;;;;;;;;;;;;

(defun shadowing-import (symbols &optional (package *package*))
  "Inserts each of symbols into package as an internal symbol, regardless of whether another symbol of the same name is
  shadowed by this action."
  (declare (system::%java-class-name "jcl.packages.functions.ShadowingImport"))
  (let ((package (find-package package))
        (symbols (symbol-listify symbols)))
    ($shadowingImport package symbols)))

;;;;;;;;;;;;;;;;;;;;;;

(defun unexport (symbols &optional (package *package*))
  "Reverts external symbols in package to internal status."
  (declare (system::%java-class-name "jcl.packages.functions.Unexport"))
  (let ((package (find-package package))
        (symbols (symbol-listify symbols)))
    ($unexport package symbols)))

;;;;;;;;;;;;;;;;;;;;;;

(defun make-package (package-name &key nicknames use)
  "Creates a new package with the name package-name."
  (declare (system::%java-class-name "jcl.packages.functions.MakePackage"))
  (let ((package-name (string package-name))
        (nicknames (package-name-list nicknames))
        (use (package-listify use)))
    (when (find-package package-name)
      (error "A package named ~S already exists." name))
    (ext:jinvoke-static
      (ext:jmethod "makePackage" (ext:jclass "jcl.lang.PackageStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.ListStruct")
                   (ext:jclass "jcl.lang.ListStruct"))
      package-name nicknames use)))

;;;;;;;;;;;;;;;;;;;;;;

(defun rename-package (package new-name &optional new-nicknames)
  "Replaces the name and nicknames of package."
  (declare (system::%java-class-name "jcl.packages.functions.RenamePackage"))
  (let ((package (find-package package))
        (name (string new-name))
        (found (find-package new-name))
        (new-nicknames (package-name-list new-nicknames)))
    (unless (or (not found) (eq found package))
      (error "A package named ~S already exists." name))
    ($renamePackage package name new-nicknames)))

;;;;;;;;;;;;;;;;;;;;;;

(defun delete-package (package)
  "Deletes package from all package system data structures. If the operation is successful, returns true, otherwise nil."
  (declare (system::%java-class-name "jcl.packages.functions.DeletePackage"))
  (let ((package (find-package package)))
    ($deletePackage package)))

;;;;;;;;;;;;;;;;;;;;;;

(defun shadow (symbol-names &optional (package *package*))
  "Assures that symbols with names given by symbol-names are present in the package."
  (declare (system::%java-class-name "jcl.packages.functions.Shadow"))
  (let ((package (find-package package))
        (symbol-names (string-listify symbol-names)))
    ($shadow package symbol-names)))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: package-error-package
#|
(defun package-error-package (condition)
  "Returns a designator for the offending package in the situation represented by the condition."
  (declare (system::%java-class-name "jcl.packages.functions.PackageErrorPackage"))

  ;; PackageErrorException packageErrorException = (PackageErrorException) condition;
  ;; return packageErrorException.getPackageWithError();

  nil)
|#
;;;;;;;;;;;;;;;;;;;;;;

;; TODO: in-package

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: defpackage

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: do-symbols, do-external-symbols, do-all-symbols

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: with-package-iterator

;;;;;;;;;;;;;;;;;;;;;;

(provide "packages")