;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defun pathname (pathspec)
  "Returns the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.Pathname"))
  (ext:jinvoke-static
    (ext:jmethod "toPathname" (ext:jclass "jcl.lang.PathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    pathspec))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: make-pathname

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: pathnamep

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: handle "case" keyword

(defun pathname-host (pathname &key (case :local))
  "Returns the pathname-host component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameHost"))
  (let ((pathname (pathname pathname)))
    ($pathnameHost pathname)))

(defun pathname-device (pathname &key (case :local))
  "Returns the pathname-device component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameDevice"))
  (let ((pathname (pathname pathname)))
    ($pathnameDevice pathname)))

(defun pathname-directory (pathname &key (case :local))
  "Returns the pathname-directory component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameDirectory"))
  (let ((pathname (pathname pathname)))
    ($pathnameDirectory pathname)))

(defun pathname-name (pathname &key (case :local))
  "Returns the pathname-name component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameName"))
  (let ((pathname (pathname pathname)))
    ($pathnameName pathname)))

(defun pathname-type (pathname &key (case :local))
  "Returns the pathname-type component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameType"))
  (let ((pathname (pathname pathname)))
    ($pathnameType pathname)))

(defun pathname-version (pathname)
  "Returns the pathname-version component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameVersion"))
  (let ((pathname (pathname pathname)))
    ($pathnameVersion pathname)))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: load-logical-pathname-translations

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: logical-pathname-translations (setf)

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: logical-pathname

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: namestring, file-namestring, directory-namestring, host-namestring, enough-namestring

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: parse-namestring

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: wild-pathname-p

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: pathname-match-p

;;;;;;;;;;;;;;;;;;;;;;

(defun translate-logical-pathname (pathname &key)
  "Translates pathname to a physical pathname, which it returns."
  (declare (system::%java-class-name "jcl.pathnames.functions.TranslateLogicalPathname"))
  (ext:jinvoke-static
    (ext:jmethod "translateLogicalPathname" (ext:jclass "jcl.lang.PathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    pathname))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: translate-pathname

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: handle default-version in Lisp rather than Java

(defun merge-pathnames (pathname &optional (default-pathname *default-pathname-defaults*) (default-version :newest))
  "Constructs a pathname from pathname by filling in any unsupplied components with the corresponding values from
  default-pathname and default-version."
  (declare (system::%java-class-name "jcl.pathnames.functions.MergePathnames"))
  (let ((pathname (pathname pathname))
        (default-pathname (pathname default-pathname))
        (default-version (ext:jinvoke-static
                           (ext:jmethod "getPathnameVersion" (ext:jclass "jcl.lang.pathname.PathnameVersion")
                                        (ext:jclass "jcl.lang.LispStruct"))
                           default-version)))
    (ext:jinvoke-static
      (ext:jmethod "mergePathnames" (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.pathname.PathnameVersion"))
      pathname default-pathname default-version)))

;;;;;;;;;;;;;;;;;;;;;;

(export '(pathname
          make-pathname
          pathnamep
          pathname-host pathname-device pathname-directory pathname-name pathname-type pathname-version
          load-logical-pathname-translations
          logical-pathname-translations
          logical-pathname
          *default-pathname-defaults*
          namestring file-namestring directory-namestring host-namestring enough-namestring
          parse-namestring
          wild-pathname-p
          pathname-match-p
          translate-logical-pathname
          translate-pathname
          merge-pathnames)
        "COMMON-LISP")

(provide "pathnames")