;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "sequences")
  (require "macros")
  (require "iterators")
  (require "symbols")
  (require "lists")
  (require "numbers")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)

(defun put (symbol indicator value)
  (ext:jinvoke-interface
    (ext:jmethod "setProp" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol indicator value))

) ;eval-when

;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)

(defun get-setf-expansion (form &optional environment)
  (declare (system::%java-class-name "jcl.common.functions.GetSetfExpansion"))
  (let (temp)
    (cond ((symbolp form)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 form environment)
             (if expanded
                 (get-setf-expansion expansion environment)
               (let ((new-var (gensym)))
                 (values nil nil (list new-var)
                         `(setq ,form ,new-var) form)))))
          ((setq temp (get (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get (car form) 'setf-expander))
           (funcall temp form environment))
          (t
           (expand-or-get-setf-inverse form environment)))))

) ;eval-when
#|
(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
        (vars nil)
        (vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
            (if setf-function
                `(,@inverse ,new-var ,@vars)
              (if (functionp (car inverse))
                  `(funcall ,@inverse ,@vars ,new-var)
                `(,@inverse ,@vars ,new-var)))
            `(,(car form) ,@vars))))
|#

(eval-when (:load-toplevel :compile-toplevel :execute)

(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
        (vars nil)
        (vals nil))
    (dolist (x (cdr form))
      (setq vars (cons (gensym) vars))
      (setq vals (cons x vals)))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
            (if setf-function
                `(,@inverse ,new-var ,@vars)
              (if (functionp (car inverse))
                  `(funcall ,@inverse ,@vars ,new-var)
                `(,@inverse ,@vars ,new-var)))
            `(,(car form) ,@vars))))

) ;eval-when

;;; If a macro, expand one level and try again. If not, go for the SETF function.
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
      (get-setf-method-inverse form `(funcall #'(setf ,(car form))) t))))

(defmacro defsetf (access-function update-function)
  (declare (system::%java-class-name "jcl.common.macros.DefSetf"))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put ',access-function 'setf-inverse ',update-function)))

(defmacro setf (&rest args &environment environment)
  (declare (system::%java-class-name "jcl.common.macros.Setf"))
  (let ((numargs (length args)))
    (cond
     ((= numargs 2)
      (let ((place (first args))
            (value-form (second args)))
        (if (atom place)
            `(setq ,place ,value-form)
          (progn
            (multiple-value-bind (dummies vals store-vars setter getter)
                (get-setf-expansion place environment)
              (let ((inverse (get (car place) 'setf-inverse)))
                (if (and inverse (eq inverse (car setter)))
                    (if (functionp inverse)
                        `(funcall ,inverse ,@(cdr place) ,value-form)
                      `(,inverse ,@(cdr place) ,value-form))
                  (if (or (null store-vars) (cdr store-vars))
                      `(let* (,@(mapcar #'list dummies vals))
                         (multiple-value-bind ,store-vars ,value-form
                           ,setter))
                    `(let* (,@(mapcar #'list dummies vals)
                            ,(list (car store-vars) value-form))
                       ,setter)))))))))
     ((oddp numargs)
      (error "Odd number of arguments to SETF."))
     (t
      (do ((a args (cddr a)) (l nil))
          ((null a) `(progn ,@(nreverse l)))
        (setq l (cons (list 'setf (car a) (cadr a)) l)))))))

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;; compiler-macro-function
;; macro-function
;; fdefinition
;; find-class

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; LISTS ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun set-car (cons object)
  (ext:jinvoke-interface
    (ext:jmethod "rplaca" (ext:jclass "jcl.lang.ConsStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    cons object))

(defun set-cdr (cons object)
  (ext:jinvoke-interface
    (ext:jmethod "rplacd" (ext:jclass "jcl.lang.ConsStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    cons object))

(defun %set-caar (x v) (%set-car (car x) v))
(defun %set-cadr (x v) (%set-car (cdr x) v))
(defun %set-cdar (x v) (%set-cdr (car x) v))
(defun %set-cddr (x v) (%set-cdr (cdr x) v))
(defun %set-caaar (x v) (%set-car (caar x) v))
(defun %set-cadar (x v) (%set-car (cdar x) v))
(defun %set-cdaar (x v) (%set-cdr (caar x) v))
(defun %set-cddar (x v) (%set-cdr (cdar x) v))
(defun %set-caadr (x v) (%set-car (cadr x) v))
(defun %set-caddr (x v) (%set-car (cddr x) v))
(defun %set-cdadr (x v) (%set-cdr (cadr x) v))
(defun %set-cdddr (x v) (%set-cdr (cddr x) v))
(defun %set-caaaar (x v) (%set-car (caaar x) v))
(defun %set-cadaar (x v) (%set-car (cdaar x) v))
(defun %set-cdaaar (x v) (%set-cdr (caaar x) v))
(defun %set-cddaar (x v) (%set-cdr (cdaar x) v))
(defun %set-caadar (x v) (%set-car (cadar x) v))
(defun %set-caddar (x v) (%set-car (cddar x) v))
(defun %set-cdadar (x v) (%set-cdr (cadar x) v))
(defun %set-cdddar (x v) (%set-cdr (cddar x) v))
(defun %set-caaadr (x v) (%set-car (caadr x) v))
(defun %set-cadadr (x v) (%set-car (cdadr x) v))
(defun %set-cdaadr (x v) (%set-cdr (caadr x) v))
(defun %set-cddadr (x v) (%set-cdr (cdadr x) v))
(defun %set-caaddr (x v) (%set-car (caddr x) v))
(defun %set-cadddr (x v) (%set-car (cdddr x) v))
(defun %set-cdaddr (x v) (%set-cdr (caddr x) v))
(defun %set-cddddr (x v) (%set-cdr (cdddr x) v))

(defsetf car set-car)
(defsetf cdr set-cdr)
(defsetf caar %set-caar)
(defsetf cadr %set-cadr)
(defsetf cdar %set-cdar)
(defsetf cddr %set-cddr)
(defsetf caaar %set-caaar)
(defsetf cadar %set-cadar)
(defsetf cdaar %set-cdaar)
(defsetf cddar %set-cddar)
(defsetf caadr %set-caadr)
(defsetf caddr %set-caddr)
(defsetf cdadr %set-cdadr)
(defsetf cdddr %set-cdddr)
(defsetf caaaar %set-caaaar)
(defsetf cadaar %set-cadaar)
(defsetf cdaaar %set-cdaaar)
(defsetf cddaar %set-cddaar)
(defsetf caadar %set-caadar)
(defsetf caddar %set-caddar)
(defsetf cdadar %set-cdadar)
(defsetf cdddar %set-cdddar)
(defsetf caaadr %set-caaadr)
(defsetf cadadr %set-cadadr)
(defsetf cdaadr %set-cdaadr)
(defsetf cddadr %set-cddadr)
(defsetf caaddr %set-caaddr)
(defsetf cadddr %set-cadddr)
(defsetf cdaddr %set-cdaddr)
(defsetf cddddr %set-cddddr)

(defun %set-fifth (x v) (%set-car (cddddr x) v))
(defun %set-sixth (x v) (%set-car (cdr (cddddr x)) v))
(defun %set-seventh (x v) (%set-car (cddr (cddddr x)) v))
(defun %set-eighth (x v) (%set-car (cdddr (cddddr x)) v))
(defun %set-ninth (x v) (%set-car (cddddr (cddddr x)) v))
(defun %set-tenth (x v) (%set-car (cdr (cddddr (cddddr x))) v))

(defsetf first set-car)
(defsetf second %set-cadr)
(defsetf third %set-caddr)
(defsetf fourth %set-cadddr)
(defsetf fifth %set-fifth)
(defsetf sixth %set-sixth)
(defsetf seventh %set-seventh)
(defsetf eighth %set-eighth)
(defsetf ninth %set-ninth)
(defsetf tenth %set-tenth)

(defsetf rest set-cdr)

(defun %set-nth (sequence index value)
  (setf (car (nthcdr n list)) new-object))

(defsetf nth %set-nth)

(defun (setf getf) (plist indicator value &optional default)
  "Finds a property on the property list whose property indicator is identical to indicator, and sets its corresponding
  property value with the new-value provided."
  (declare (system::%java-class-name "jcl.lists.functions.SetfGetf")
           (ignore default))
  (ext:jinvoke-interface
    (ext:jmethod "putf" (ext:jclass "jcl.lang.ListStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    plist indicator value))

;;;;;;;;;;;;;;;;;;;;;;
;;; ARRAYS/VECTORS ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun %set-aref (array value &rest subscripts)
  (ext:jinvoke-interface
    (ext:jmethod "setfAref" (ext:jclass "jcl.lang.ArrayStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "[Ljcl.lang.IntegerStruct;"))
    array value subscripts))

(defun %set-bit (vector value index)
  (ext:jinvoke-interface
    (ext:jmethod "setfBit" (ext:jclass "jcl.lang.BitArrayStruct")
                 (ext:jclass "jcl.lang.FixnumStruct")
                 (ext:jclass "[Ljcl.lang.IntegerStruct;"))
    vector value index))

(defun %set-sbit (vector value index)
  (ext:jinvoke-interface
    (ext:jmethod "setfSbit" (ext:jclass "jcl.lang.BitArrayStruct")
                 (ext:jclass "jcl.lang.FixnumStruct")
                 (ext:jclass "[Ljcl.lang.IntegerStruct;"))
    vector value index))

(defun %set-svref (vector value index)
  (ext:jinvoke-interface
    (ext:jmethod "setfSvref" (ext:jclass "jcl.lang.VectorStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.FixnumStruct"))
    vector value index))

(defun %set-fill-pointer (vector fill-pointer)
  (ext:jinvoke-interface
    (ext:jmethod "setfFillPointer" (ext:jclass "jcl.lang.VectorStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    vector fill-pointer))

(defun %set-char (string value index)
  (ext:jinvoke-interface
    (ext:jmethod "setfChar" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.FixnumStruct"))
    string value index))

(defun %set-schar (string value index)
  (ext:jinvoke-interface
    (ext:jmethod "setfSchar" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.FixnumStruct"))
    string value index))

(defsetf row-major-aref %set-aref)
(defsetf aref %set-aref)

(defsetf bit %set-bit)
(defsetf sbit %set-sbit)

(defsetf svref %set-svref)
(defsetf fill-pointer %set-fill-pointer)

(defsetf char %set-char)
(defsetf schar %set-schar)

;;;;;;;;;;;;;;;;;;;;;;
;;;;; SEQUENCES ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun %set-elt (sequence index value)
  (ext:jinvoke-interface
    (ext:jmethod "setfElt" (ext:jclass "jcl.lang.SequenceStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    sequence value index))

(defsetf elt %set-elt)

(defun %set-subseq (sequence start &rest rest)
  (let ((end nil) v)
    (ecase (length rest)
      (1
       (setq v (car rest)))
      (2
       (setq end (car rest)
             v (cadr rest))))
    (progn
      (replace sequence v :start1 start :end1 end)
      v)))

(defsetf subseq %set-subseq)

;;;;;;;;;;;;;;;;;;;;;;
;;;;;; SYMBOLS ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun %set-symbol-value (symbol new-value)
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    symbol new-value))

(defun %set-symbol-function (symbol new-contents)
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolFunction" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.FunctionStruct"))
    symbol new-contents))

(defun %set-symbol-plist (symbol new-plist)
  (ext:jinvoke-interface
    (ext:jmethod "setfSymbolPlist" (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.ListStruct"))
    symbol new-plist))

(defsetf symbol-value %set-symbol-value)
(defsetf symbol-function %set-symbol-function)
(defsetf symbol-plist %set-symbol-plist)

(defsetf get put)

;;;;;;;;;;;;;;;;;;;;;;
;;;; HASH-TABLES ;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun puthash (new-value key hash-table)
  (ext:jinvoke-interface
    (ext:jmethod "putHash" (ext:jclass "jcl.lang.HashTableStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    hash-table key new-value))

(defsetf gethash puthash)

;;;;;;;;;;;;;;;;;;;;;;
;;;;; PATHNAMES ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun %set-logical-pathname-translations (host translations)
  (setf host (canonicalize-logical-host host))
  ;; Avoid undefined host error in CANONICALIZE-LOGICAL-PATHNAME-TRANSLATIONS.
  (unless (logical-host-p host)
    (setf (gethash host *logical-pathname-translations*) nil))
  (setf (gethash host *logical-pathname-translations*)
        (canonicalize-logical-pathname-translations translations host)))

(defsetf logical-pathname-translations %set-logical-pathname-translations)

;;;;;;;;;;;;;;;;;;;;;;
;;;;; READTABLES ;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun %set-readtable-case (readtable new-case)
  (ext:jinvoke-interface
    (ext:jmethod "setReadtableCase" (ext:jclass "jcl.lang.ReadtableStruct")
                 (ext:jclass "jcl.lang.SymbolStruct"))
    readtable new-case))

(defsetf readtable-case %set-readtable-case)

;;;;;;;;;;;;;;;;;;;;;;
;;;;;; STREAMS ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun %set-stream-external-format (file-stream new-external-format)
  (ext:jinvoke-interface
    (ext:jmethod "setfStreamExternalFormat" (ext:jclass "jcl.lang.FileStreamStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    file-stream new-external-format))

(defsetf stream-external-format %set-stream-external-format)

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(defmacro push (item place &environment env)
  "Takes an object and a location holding a list.  Conses the object onto
   the list, returning the modified list.  OBJ is evaluated before PLACE."
  (declare (system::%java-class-name "jcl.lists.macros.Push"))
  (if (and (symbolp place)
           (eq place (macroexpand place env)))
      `(setq ,place (cons ,item ,place))
    (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
      (let ((g (gensym)))
        `(let* ((,g ,item)
                ,@(mapcar #'list dummies vals)
                (,(car newval) (cons ,g ,getter)))
           ,setter)))))

(defmacro pushnew (item place &rest keys &environment env)
  "Takes an object and a location holding a list.  If the object is already
   in the list, does nothing.  Else, conses the object onto the list.  Returns
   NIL.  If there is a :TEST keyword, this is used for the comparison."
  (declare (system::%java-class-name "jcl.lists.macros.Pushnew"))
  (if (and (symbolp place)
           (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,item ,place ,@keys))
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
      (let ((g (gensym)))
        `(let* ((,g ,item)
                ,@(mapcar #'list dummies vals)
                (,(car newval) (adjoin ,g ,getter ,@keys)))
           ,setter)))))
#|
(defmacro pop (place &environment env)
  "The argument is a location holding a list.  Pops one item off the front
   of the list and returns it."
  (declare (system::%java-class-name "jcl.lists.macros.Pop"))
  (if (and (symbolp place)
           (eq place (macroexpand place env)))
      `(prog1 (car ,place)
         (setq ,place (cdr ,place)))
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
      (do* ((d dummies (cdr d))
            (v vals (cdr v))
            (let-list nil))
           ((null d)
            (push (list (car newval) getter) let-list)
            `(let* ,(nreverse let-list)
               (prog1 (car ,(car newval))
                 (setq ,(car newval) (cdr ,(car newval)))
                 ,setter)))
        (push (list (car d) (car v)) let-list)))))

(defmacro psetq (&environment env &rest args)
  "Assigns values to variables. This is just like setq, except that the assignments happen ``in parallel.'' That is,
   first all of the forms are evaluated, and only then are the variables set to the resulting values. In this way,
   the assignment to one variable does not affect the value computation of another in the way that would occur with
   setq's sequential assignment."
  (declare (system::%java-class-name "jcl.lists.macros.Psetq"))
  (do ((l args (cddr l))
       (forms nil)
       (bindings nil))
    ((endp l) (list* 'let* (reverse bindings) (reverse (cons nil forms))))
    (if (and (symbolp (car l))
             (eq (car l) (macroexpand-1 (car l) env)))
        (let ((sym (gensym)))
          (push (list sym (cadr l)) bindings)
          (push (list 'setq (car l) sym) forms))
        (multiple-value-bind (dummies vals newval setter getter)
            (get-setf-expansion (macroexpand-1 (car l) env) env)
          (declare (ignore getter))
          (do ((d dummies (cdr d))
               (v vals (cdr v)))
              ((null d))
            (push (list (car d) (car v)) bindings))
          (push (list (car newval) (cadr l)) bindings)
          (push setter forms)))))

(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
   to hold a property list or (). This list is destructively altered to
   remove the property specified by the indicator. Returns T if such a
   property was present, NIL if not."
  (declare (system::%java-class-name "jcl.lists.macros.Remf"))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (do* ((d dummies (cdr d))
          (v vals (cdr v))
          (let-list nil)
          (ind-temp (gensym))
          (local1 (gensym))
          (local2 (gensym)))
     ((null d)
          ;; See ANSI 5.1.3 for why we do out-of-order evaluation
      (push (list ind-temp indicator) let-list)
      (push (list (car newval) getter) let-list)
      `(let* ,(nreverse let-list)
         (do ((,local1 ,(car newval) (cddr ,local1))
              (,local2 nil ,local1))
           ((atom ,local1) nil)
           (cond ((atom (cdr ,local1))
                  (error "Odd-length property list in REMF."))
                 ((eq (car ,local1) ,ind-temp)
                  (cond (,local2
                         (rplacd (cdr ,local2) (cddr ,local1))
                         (return t))
                        (t (setq ,(car newval) (cddr ,(car newval)))
                           ,setter
                           (return t))))))))
      (push (list (car d) (car v)) let-list))))
|#
;;;;;;;;;;;;;;;;;;;;;;

(defmacro incf (place &optional (delta 1))
  "Increments the value of place; the is added to the number in place and the result is stored in place."
  (declare (system::%java-class-name "jcl.numbers.macros.Incf"))
  `(setf ,place (+ ,place ,delta)))

(defmacro decf (place &optional (delta 1))
  "Decrements the value of place; the is subtracted from the number in place and the result is stored in place."
  (declare (system::%java-class-name "jcl.numbers.macros.Decf"))
  `(setf ,place (- ,place ,delta)))

;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro define-setf-expander (access-fn lambda-list &body body)
  (declare (system::%java-class-name "jcl.common.macros.DefineSetfExpander"))
  (require-type access-fn 'symbol)
  (let ((whole (gensym "WHOLE-"))
        (environment (gensym "ENV-")))
    (multiple-value-bind (body local-decs doc)
                         (parse-defmacro lambda-list whole body access-fn
                                         'define-setf-expander
                                         :environment environment)
      `(progn
         (record-source-information-for-type ',access-fn :setf-expander)
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(when doc
               `((%set-documentation ',access-fn 'setf ,doc)))
           (setf (get ',access-fn 'setf-expander)
                 #'(lambda (,whole ,environment)
                     ,@local-decs
                     (block ,access-fn ,body)))
           ',access-fn)))))

(define-setf-expander values (&rest places &environment env)
  (let ((setters ())
        (getters ())
        (all-dummies ())
        (all-vals ())
        (newvals ()))
    (dolist (place places)
      (multiple-value-bind (dummies vals newval setter getter)
          (get-setf-expansion place env)
        (setf all-dummies (append all-dummies dummies (cdr newval))
              all-vals (append all-vals vals
                               (mapcar (constantly nil) (cdr newval)))
              newvals (append newvals (list (car newval))))
        (push setter setters)
        (push getter getters)))
    (values all-dummies all-vals newvals
            `(values ,@(reverse setters)) `(values ,@(reverse getters)))))

(define-setf-expander getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemp (gensym))
          (def-temp (if default (gensym))))
      (values `(,@temps ,ptemp ,@(if default `(,def-temp)))
              `(,@values ,prop ,@(if default `(,default)))
              `(,newval)
              `(let ((,(car stores) (%putf ,get ,ptemp ,newval)))
                 ,set
                 ,newval)
              `(getf ,get ,ptemp ,@(if default `(,def-temp)))))))

(define-setf-expander ldb (bytespec place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (if (and (consp bytespec) (eq (car bytespec) 'byte))
        (let ((n-size (gensym))
              (n-pos (gensym))
              (n-new (gensym)))
          (values (list* n-size n-pos dummies)
                  (list* (second bytespec) (third bytespec) vals)
                  (list n-new)
                  `(let ((,(car newval) (dpb ,n-new (byte ,n-size ,n-pos) ,getter)))
                     ,setter
                     ,n-new)
                  `(ldb (byte ,n-size ,n-pos) ,getter)))
      (let ((btemp (gensym))
            (gnuval (gensym)))
        (values (cons btemp dummies)
                (cons bytespec vals)
                (list gnuval)
                `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
                   ,setter
                   ,gnuval)
                `(ldb ,btemp ,getter))))))

(defun make-gensym-list (n)
  (let ((list ()))
    (dotimes (i n list)
      (push (gensym) list))))

(define-setf-expander apply (functionoid &rest args)
  (let ((function (second functionoid))
        (new-var (gensym))
        (vars (make-gensym-list (length args))))
    (values vars args (list new-var)
            `(apply #'(setf ,function) ,new-var ,@vars)
            `(apply #',function ,@vars))))

(define-setf-expander the (type place &environment env)
  (multiple-value-bind (temps subforms store-vars setter getter)
      (get-setf-expansion place env)
    (values temps subforms store-vars
            `(multiple-value-bind ,store-vars
               (the ,type (values ,@store-vars))
               ,setter)
            `(the ,type ,getter))))
|#
;;;;;;;;;;;;;;;;;;;;;;

(provide "setf")
