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
    (ext:jmethod "fromDesignator" (ext:jclass "jcl.lang.PathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    pathspec))

;;;;;;;;;;;;;;;;;;;;;;

(defun make-pathname (&key (host nil supplied-host-p)
                           (device nil supplied-device-p)
                           (directory nil supplied-directory-p)
                           (name nil supplied-name-p)
                           (type nil supplied-type-p)
                           (version nil supplied-version-p)
                           (defaults *default-pathname-defaults*)
                           (case :local))
  "Constructs a pathname from pathname by filling in any unsupplied components with the corresponding values from
  default-pathname and default-version."
  (declare (system::%java-class-name "jcl.pathnames.functions.MakePathname"))
  (let ((host (if supplied-host-p host (pathname-host defaults)))
        (device (if supplied-device-p device (pathname-device defaults)))
        (directory (if supplied-directory-p directory (pathname-directory defaults)))
        (name (if supplied-name-p name (pathname-name defaults)))
        (type (if supplied-type-p type (pathname-type defaults)))
        (version (if supplied-version-p version (pathname-version defaults))))
    (ext:jinvoke-static
      (ext:jmethod "toPathname" (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.LispStruct")
                   (ext:jclass "jcl.lang.LispStruct")
                   (ext:jclass "jcl.lang.LispStruct")
                   (ext:jclass "jcl.lang.LispStruct")
                   (ext:jclass "jcl.lang.LispStruct")
                   (ext:jclass "jcl.lang.LispStruct"))
      host device directory name type version)))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: pathnamep

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: handle "case" keyword

(defun pathname-host (pathname &key (case :local))
  "Returns the pathname-host component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameHost"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameHost" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun pathname-device (pathname &key (case :local))
  "Returns the pathname-device component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameDevice"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameDevice" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun pathname-directory (pathname &key (case :local))
  "Returns the pathname-directory component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameDirectory"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameDirectory" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun pathname-name (pathname &key (case :local))
  "Returns the pathname-name component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameName"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameName" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun pathname-type (pathname &key (case :local))
  "Returns the pathname-type component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameType"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameType" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun pathname-version (pathname)
  "Returns the pathname-version component of the pathname denoted by pathspec."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameVersion"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameVersion" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

;;;;;;;;;;;;;;;;;;;;;;
#|
(defun load-logical-pathname-translations (host)
  "Searches for and loads the definition of a logical host named host, if it is not already defined.."
  (declare (system::%java-class-name "jcl.pathnames.functions.LoadLogicalPathnameTranslations"))
  (ext:jinvoke-static
    (ext:jmethod "logicalPathnameTranslations" (ext:jclass "jcl.lang.LogicalPathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    host))
|#
;;;;;;;;;;;;;;;;;;;;;;
#|
(defun logical-pathname-translations (host)
  "Returns the host's list of translations."
  (declare (system::%java-class-name "jcl.pathnames.functions.LogicalPathnameTranslations"))
  (ext:jinvoke-static
    (ext:jmethod "logicalPathnameTranslations" (ext:jclass "jcl.lang.LogicalPathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    host))

(defun (setf logical-pathname-translations) (host new-translations)
  "Sets the host's list of translations to the new-value provided."
  (declare (system::%java-class-name "jcl.pathnames.functions.SetfLogicalPathnameTranslations"))
  (ext:jinvoke-static
    (ext:jmethod "setLogicalPathnameTranslations" (ext:jclass "jcl.lang.LogicalPathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    host new-translations))
|#
;;;;;;;;;;;;;;;;;;;;;;

(defun logical-pathname (pathspec)
  "Converts pathspec to a logical pathname and returns the new logical pathname."
  (declare (system::%java-class-name "jcl.pathnames.functions.LogicalPathname"))
  (ext:jinvoke-static
    (ext:jmethod "fromDesignator" (ext:jclass "jcl.lang.LogicalPathnameStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    pathspec))

;;;;;;;;;;;;;;;;;;;;;;

(defun namestring (pathname)
  "Returns the full form of pathname."
  (declare (system::%java-class-name "jcl.pathnames.functions.Namestring"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "namestring" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun file-namestring (pathname)
  "Returns just the name, type, and version components of pathname."
  (declare (system::%java-class-name "jcl.pathnames.functions.FileNamestring"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "fileNamestring" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun directory-namestring (pathname)
  "Returns the directory name portion."
  (declare (system::%java-class-name "jcl.pathnames.functions.DirectoryNamestring"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "directoryNamestring" (ext:jclass "jcl.lang.PathnameStruct"))
      pathname)))

(defun host-namestring (pathname)
  "Returns the host name."
  (declare (system::%java-class-name "jcl.pathnames.functions.HostNamestring"))
  (let ((pathname (pathname pathname)))
    (pathname-host pathname)))
#| TODO
(declaim (inline equal-components-p))
(defun equal-components-p (component1 component2)
  #+win32 (equalp component1 component2)
  #-win32 (equal component1 component2))

(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
  (unless (equal (pathname-host pathname) (pathname-host defaults))
    (return-from enough-namestring (namestring pathname)))
  (let ((pathname-directory (pathname-directory pathname)))
    (if pathname-directory
        (let* ((defaults-directory (pathname-directory defaults))
               (prefix-len (length defaults-directory))
               (result-directory
                (cond ((and (>= prefix-len 1)
                            (>= (length pathname-directory) prefix-len)
                            (equal-components-p (subseq pathname-directory 0 prefix-len) defaults-directory))
                       (cons :relative (nthcdr prefix-len pathname-directory)))
                      ((eq (car pathname-directory) :absolute)
                       pathname-directory)
                      (t
                       (return-from enough-namestring (namestring pathname))))))
          (if (equal result-directory '(:relative))
              (file-namestring pathname)
              (concatenate 'simple-string
                           (directory-namestring (make-pathname :directory result-directory))
                           (file-namestring pathname))))
        (file-namestring pathname))))
|#
;;;;;;;;;;;;;;;;;;;;;;

#| TODO
(defun parse-namestring (thing
                         &optional host (default-pathname *default-pathname-defaults*)
                         &key (start 0) end junk-allowed)
  (declare (ignore junk-allowed)) ; FIXME
  (cond ((eq host :unspecific)
         (setf host nil))
        ((consp host)) ;; A URL
        (host
         (setf host (canonicalize-logical-host host))))
  (typecase thing
    (stream
     (values (pathname thing) start))
    (pathname
     (values thing start))
    (string
     (unless end
       (setf end (length thing)))
     (%parse-namestring (subseq thing start end) host default-pathname))
    (t
     (error 'type-error
            :format-control "~S cannot be converted to a pathname."
            :format-arguments (list thing)))))
|#
;;;;;;;;;;;;;;;;;;;;;;

(defun wild-pathname-p (pathname &optional field-key)
  "Tests pathname for the presence of wildcard components."
  (declare (system::%java-class-name "jcl.pathnames.functions.WildPathnameP"))
  (let ((pathname (pathname pathname)))
    (ext:jinvoke-interface
      (ext:jmethod "wildPathnameP" (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.LispStruct"))
      pathname field-key)))

;;;;;;;;;;;;;;;;;;;;;;
#|
(defun pathname-match-p (pathname wildcard)
  "Returns true if pathname matches wildcard, otherwise nil. Missing components of wildcard default to :wild."
  (declare (system::%java-class-name "jcl.pathnames.functions.PathnameMatchP"))
  (let ((pathname (pathname pathname))
        (wildcard (pathname wildcard)))
    (ext:jinvoke-interface
      (ext:jmethod "pathnameMatchP" (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.PathnameStruct"))
      pathname wildcard)))
|#
;;;;;;;;;;;;;;;;;;;;;;

(defun translate-logical-pathname (pathname &key)
  "Translates pathname to a physical pathname, which it returns."
  (declare (system::%java-class-name "jcl.pathnames.functions.TranslateLogicalPathname"))
  (typecase pathname
    (logical-pathname
     (let* ((host (pathname-host pathname))
            (translations (logical-pathname-translations host)))
       (dolist (translation translations
                            (error 'file-error
                                   :pathname pathname
                                   :format-control "No translation for ~S"
                                   :format-arguments (list pathname)))
         (let ((from-wildcard (car translation))
               (to-wildcard (cadr translation)))
           (when (pathname-match-p pathname from-wildcard)
             (return (translate-logical-pathname
                      (translate-pathname pathname from-wildcard to-wildcard))))))))
    (pathname pathname)
    (t
     (translate-logical-pathname (pathname pathname)))))

;;;;;;;;;;;;;;;;;;;;;;
#|
(defun translate-pathname (source from-wildcard to-wildcard &key)
  "Translates source (that matches from-wildcard) into a corresponding pathname that matches to-wildcard, and returns the corresponding pathname."
  (declare (system::%java-class-name "jcl.pathnames.functions.TranslatePathname"))
  (let ((source (pathname source))
        (from-wildcard (pathname from-wildcard))
        (to-wildcard (pathname to-wildcard)))
    (ext:jinvoke-static
      (ext:jmethod "translatePathname" (ext:jclass "jcl.lang.PathnameStructs")
                   (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.PathnameStruct")
                   (ext:jclass "jcl.lang.PathnameStruct"))
      source from-wildcard to-wildcard)))
|#
;;;;;;;;;;;;;;;;;;;;;;
#|
(defun merge-directories (dir1 dir2)
  (if (or (eq (car dir1) :absolute)
          (null dir2))
      dir1
    (let ((results nil))
      (flet ((add (dir)
               (if (and (eq dir :back)
                        (cdr results)
                        (not (eq (car results) :back)))
                   (pop results)
                 (push dir results))))
        (dolist (dir dir2)
          (add dir))
        (dolist (dir (cdr dir1))
          (add dir)))
     (reverse results))))
|#

;; TODO: should really use above when we fix push/pop
(defun merge-directories (dir1 dir2)
  (ext:jinvoke-static
    (ext:jmethod "mergeDirectories" (ext:jclass "jcl.lang.PathnameStruct")
                 (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    dir1 dir2))

(defun merge-pathnames (pathname &optional (default-pathname *default-pathname-defaults*) (default-version :newest))
  "Construct a filled in pathname by completing the unspecified components from the defaults."
  (declare (system::%java-class-name "jcl.pathnames.functions.MergePathnames"))
  (let ((pathname (pathname pathname))
        (default-pathname (pathname default-pathname)))
    (make-pathname
     :host (or (pathname-host pathname)
               (pathname-host default-pathname))
     :device (or (pathname-device pathname)
                 (pathname-device default-pathname))
     :directory (merge-directories (pathname-directory pathname)
                                   (pathname-directory default-pathname))
     :name (or (pathname-name pathname)
               (pathname-name default-pathname))
     :type (or (pathname-type pathname)
               (pathname-type default-pathname))
     :version (or (if (null (pathname-name pathname))
                      (or (pathname-version pathname)
                          (pathname-version default-pathname))
                    (pathname-version pathname))
                  default-version))))

;;;;;;;;;;;;;;;;;;;;;;

(provide "pathnames")