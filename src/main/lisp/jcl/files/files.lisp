;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "pathnames")
) ;eval-when

(in-package "COMMON-LISP")

;; TODO: should do the 'merge-pathnames' functionality here? It's currently needed to work correctly, but feels a bit off

;;;;;;;;;;;;;;;;;;;;;;

(defun file-write-date (pathspec)
  "Return the write date of the file specified by PATHSPEC.
   An error of type FILE-ERROR is signaled if no such file exists,
   or if PATHSPEC is a wild pathname."
  (declare (system::%java-class-name "jcl.files.functions.FileWriteDate"))
  (let ((pathname (pathname pathname)))
    (when (logical-pathname-p pathname)
      (setq pathname (translate-logical-pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "fileWriteDate" (ext:jclass "jcl.lang.PathnameStruct"))
      (merge-pathnames pathname *default-pathname-defaults* nil))))

;;;;;;;;;;;;;;;;;;;;;;

(defun delete-file (pathspec)
  "Delete the specified file."
  (declare (system::%java-class-name "jcl.files.functions.DeleteFile"))
  (let ((pathname (merge-pathnames pathname)))
    (when (logical-pathname-p pathname)
      (setq pathname (translate-logical-pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "deleteFile" (ext:jclass "jcl.lang.PathnameStruct"))
      (merge-pathnames pathname *default-pathname-defaults* nil))))

;;;;;;;;;;;;;;;;;;;;;;

(defun truename (pathspec)
  "Tries to find the file indicated by filespec and returns its truename."
  (declare (system::%java-class-name "jcl.files.functions.Truename"))
  (let ((pathname (pathname pathname)))
    (when (logical-pathname-p pathname)
      (setq pathname (translate-logical-pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "truename" (ext:jclass "jcl.lang.PathnameStruct"))
      (merge-pathnames pathname *default-pathname-defaults* nil))))

;;;;;;;;;;;;;;;;;;;;;;

(defun probe-file (pathspec)
  "Tests whether a file exists."
  (declare (system::%java-class-name "jcl.files.functions.ProbeFile"))
  (if (null pathspec)
      pathspec
    (let ((pathname (pathname pathname)))
      (when (logical-pathname-p pathname)
        (setq pathname (translate-logical-pathname pathname)))
      (ext:jinvoke-interface
        (ext:jmethod "probeFile" (ext:jclass "jcl.lang.PathnameStruct"))
        (merge-pathnames pathname *default-pathname-defaults* nil)))))

;;;;;;;;;;;;;;;;;;;;;;

(defun directory (pathspec)
  "Determines which, if any, files that are present in the file system have names matching pathspec, and returns a
  fresh list of pathnames corresponding to the truenames of those files."
  (declare (system::%java-class-name "jcl.files.functions.Directory"))
  (let ((pathname (merge-pathnames pathspec)))
    (when (logical-pathname-p pathname)
      (setq pathname (translate-logical-pathname pathname)))
    (let ((truename (probe-file pathname)))
      (if truename
          (list (pathname truename))
          nil))))

;;;;;;;;;;;;;;;;;;;;;;

(provide "files")