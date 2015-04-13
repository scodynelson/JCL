;;;; Copyright (c) 2011 College of Charleston, Charleston, SC, USA

;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is furnished to do
;;; so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; package COMMON-LISP
(common-lisp:in-package "COMMON-LISP")

(eval-when (:compile-toplevel :load-toplevel :execute)
(require "CoreFunctionsLsp")
(require "LambdaListParsersLsp")
(require "PrinterLsp")
(require "StreamsLsp")
(require "ArraysLsp")
(require "StringsLsp")
(require "HashTablesLsp")
(require "MiscMacrosLsp")
(require "ListsLsp")
(require "IteratorsLsp")
(require "SetfLsp")
(require "SequencesLsp")
(require "BackquoteLsp")
) ;eval-when

(export '(defstruct)
        (find-package "COMMON-LISP"))

(in-package "DEFSTRUCT")

#|
TODO: (not necessarily in priority order)
Get Doc-strings to work. For now, we just find it and ignore it.
Uncomment all type checking sections: the little parts with subtypep
    This will have until we have a proper type system
Overall clean up of file when things get fixed:
    typecase use with "otherwise"
    problem with case macro
    problem with constantp and simple-string types
    all hacks with "let" or "let*" dealing with compiler issues
Expand printer-objects (must wait for CLOS to be implemented)
|#

#|
Syntax for calling system defstruct stuff
(compiler::%defstruct (java-name struct-name included-struct-name) (a :type blah) (b :type blah))
(system::%make-defstruct-instance (list ('javastructname) value1 value2 value3 ...)))
(system::%get-defstruct-slots instance slot-name)
(system::%get-defstruct-slot instance slot-name)
(system::%set-defstruct-slot instance slot-name slot-value)
(system::%get-defstruct-parent struct-name)
(system::%set-defstruct-parent struct-name new-parent)
(system::%get-defstruct-slot-list struct-name)
(system::%set-defstruct-slot-list struct-name new-slot-list)
(system::%get-defstruct-total-slot-number struct-name)
(system::%set-defstruct-total-slot-number struct-name new-total-slot-number)
(system::%get-defstruct-type struct-name)
(system::%get-defstruct-named-list struct-name)
(system::%set-defstruct-named-list struct-name new-named-list)
(system::%get-defstruct-default-constructor struct-name)
(system::%set-defstruct-default-constructor struct-name new-constructor-name)
(system::%copy-struct struct-instance)
|#

;;; List of all valid defstruct option keywords
(defvar *defstruct-valid-options*
  '(:conc-name :constructor :copier :include :initial-offset :named :predicate :type :print-object :print-function))

;;; List of defstruct options to be filled in if not explicitly
;;; supplied.  Does not include :NAMED because this simply exists or
;;; does not; it has no value.  Printer options are handled
;;; separately when SET-PRINT-OPTION-VALUE is called. :CONC-NAME is
;;; handled when SET-CONC-NAME-VALUE is called.
(defvar *defstruct-fill-options*
  '(:constructor :copier :include :initial-offset :predicate :type))

;;; List of defstruct option keywords that have to be checked for BNF grammar compliance
;;; These options may not exist as naked symbols (they must be in a list)
(defvar *non-naked-option-keys*
  '(:print-object :print-function :include :type :initial-offset))

;;; The defstruct macro. It doesn't really do much. It calls a bunch of
;;; other functions to check args, get default values, build expansions, etc.
(defmacro common-lisp:defstruct (name-and-options &body slots)
  (declare (system::%java-class-name "lisp.common.function.Defstruct"))
  (let* ((struct-name (if (listp name-and-options) (car name-and-options) name-and-options))
         (options (if (listp name-and-options) (cdr name-and-options) nil))
         (java-name (javafy-struct-name struct-name))
         (p-options (process-options-list struct-name options))
         (v-options (set-option-values struct-name p-options))
         (doc-string (get-doc-string slots))
         (slots-no-doc (if doc-string (delete doc-string slots :test #'equal) slots))
         (slots-list (process-slots-list struct-name slots-no-doc)))
  `(funcall #'(lambda ()
    ,(if (eql 'list (second (system::%assoc :type v-options)))
        (expand-list struct-name java-name v-options slots-list)
      (if (eql 'vector (second (system::%assoc :type v-options)))
          (expand-vector struct-name java-name v-options slots-list)
        (expand-regular struct-name java-name v-options slots-list))))) ))

;;;BELOW IS SOMETHING WRONG WHEN COMPILING CASE EXPRESSION. DON'T USE UNTIL PROBLEM HAS BEEN NOTABLY FIXED;;;
#|
    ;; Call correct expander based on :type of struct
    (case (second (system::%assoc :type v-options))
      (list (expand-list struct-name java-name v-options slots-list))
      (vector (expand-vector struct-name java-name v-options slots-list))
      (t (expand-regular struct-name java-name v-options slots-list)))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular expansions (i.e. not list or vector expansion)         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Master function to build all the expansions required for a regular
;;; struct. It calls various helper functions which build specific
;;; parts of the expansion (i.e. the constructors, accessors, etc.)
(defun expand-regular (struct-name java-name options slots-list)
  (let* ((x 1) ;; a hack to patch over a compiler bug
         (constructors (get-constructors options))
         (defstruct-slot-names (get-defstruct-slot-names slots-list))
         (init-forms (get-init-forms slots-list))
         (print-function-function (second (system::%assoc :print-function options)))
         (print-object-function (second (system::%assoc :print-object options)))
         (printer-to-pass nil)
         (included-struct-name (first (get-included-struct options)))
         (included-slot-number 0)
         (included-slots-list nil)
         (included-init-forms nil))

    (when print-function-function (setq printer-to-pass print-function-function))
    (when print-object-function (setq printer-to-pass print-object-function))

    (system::%set-defstruct-slot-list struct-name slots-list)
    (system::%set-defstruct-total-slot-number struct-name (length slots-list))
    (system::%set-defstruct-parent-name struct-name included-struct-name)
    (system::%set-defstruct-default-constructor struct-name (cadar constructors))
    (when included-struct-name
      ;(when (subtypep included-struct-name struct-name)
      ;  (error "~S : Cannot include the structure type itself or one of its subtypes: ~S." struct-name included-struct-name))

      (setq included-slot-number (system::%get-defstruct-total-slot-number included-struct-name))
      (system::%set-defstruct-total-slot-number struct-name (+ (length slots-list) included-slot-number))

      (setq included-slots-list (get-included-slots-list struct-name options included-struct-name))
      (system::%set-defstruct-slot-list struct-name (append included-slots-list slots-list))

      (setq included-init-forms (get-init-forms included-slots-list))

      (setq init-forms (append included-init-forms init-forms))
      (setf slots-list (append included-slots-list slots-list)))
;(eval-when (:compile-toplevel)
;(print "the first mapcan.. init-forms: ")(prin1 init-forms)(terpri)
;)
    ;; OK, this is a hack. The init-forms may contain constant values. These don't require
    ;; binding and can appear directly as constants in the MAKE- form.
    `(let (,@(mapcan #'(lambda (form) (unless (constantp (second form)) (list form))) init-forms))
       (compiler::%defstruct (,(intern java-name) ,struct-name ,included-struct-name) ,printer-to-pass ,included-slot-number ,@defstruct-slot-names)
       ,@(expand-regular-constructors (intern java-name) constructors slots-list init-forms)
       ,@(expand-regular-readers slots-list (second (system::%assoc :conc-name options)))
       ,@(expand-regular-predicate struct-name (second (system::%assoc :predicate options)))
       ;,@(expand-regular-printer java-name (second (system::%assoc :print-object options)) :print-object)
       ;,@(expand-regular-printer java-name (second (system::%assoc :print-function options)) :print-function)
       ,@(expand-regular-copier (second (system::%assoc :copier options)))
       ,@(expand-regular-setters struct-name slots-list)
       ',struct-name)))

;;; This function creates the expansion for the constructor (e.g. MAKE-FOO).
;;; It recursively goes through the list of constructors and creates a constructor
;;; for each constructor option that was supplied when defstruct was called.
(defun expand-regular-constructors (struct-name constructors slots-list init-forms)
  (when constructors
    (let* ((slot-symbols (get-slot-symbols slots-list))
           (slot-values (get-slot-values slots-list))
           (constructor (car constructors))
           (constructor-name (cadr constructor))
           (constructor-args (or (caddr constructor) `,(get-default-arglist slot-symbols slot-values init-forms))))
      (cons `(defun ,constructor-name ,constructor-args
               ,@(expand-reg-constructor-decls slots-list)
               (system::%make-defstruct-instance (list (quote ,struct-name) ,@(mapcar #'second slot-symbols))))
            (expand-regular-constructors struct-name (rest constructors) slots-list init-forms)))))

(defun expand-reg-constructor-decls (slots-list)
  (when slots-list
    (let* ((slot (car slots-list))
           (slot-name (car slot))
           (slot-value (cadr slot))
           (slot-type (system::%get-plist slot :type)))
      ;(when (subtypep (type-of slot-value) slot-type)
      ;  (error "The value ~S does not satisfy the type specifier ~S." slot-value slot-type))
      (cons `(declare (type ,slot-type ,slot-name))
            (expand-reg-constructor-decls (cdr slots-list))))))

;;; This function creates the expansion for the reader functions (e.g. FOO-A, FOO-B, etc.)
;;; It recursively goes through the slots list and sets up an accessor function for each slot.
(defun expand-regular-readers (slots conc-name)
  (when slots
    (let* ((slot (car slots))
           (slot-name (car slot)))
      (cons `(defun ,(intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name))) (struct-instance)
               (system::%get-defstruct-slot struct-instance ',slot-name))
            (expand-regular-readers (cdr slots) conc-name)))))

;;; Builds the expansion for the predicate function (e.g. FOO-P)
(defun expand-regular-predicate (struct-name pred-name)
  (when pred-name
    `((defun ,pred-name (instance)
        (eql (quote ,struct-name) (type-of instance))))))

;;; Adds an internal option, :PRINT-OPTION, whose value is a list of what print-options
;;; were supplied, or nil if none. :PRINT-OPTION is not a valid keyword to be supplied
;;; by the user. The user supplies either :PRINT-OBJECT XOR :PRINT-FUNCTION and we keep
;;; track of which one it is and its arguments internally with :PRINT-OPTION. These options
;;; involve creating print-objects which we cannot do until the CLOS is complete. The
;;; full version is located below, but commented out.
;;; NOTE: this correct version has not been tested and should be accordingly when CLOS is complete.
#|
(defun expand-regular-printer (struct-name printer kind)
  (when printer
    (let ((func (if (symbolp printer) `',printer `#',printer)))
      `((defmethod print-object ((object ,struct-name) stream)
          (funcall ,func object stream ,@(when (eq kind :print-function)
                                           '(*print-level*))))))))
|#
;;; Builds the expansion for the copier function (e.g. COPY-FOO)
(defun expand-regular-copier (copier-name)
  (when copier-name
    `((defun ,copier-name (instance)
        (system::%copy-struct instance)))))

;;; This function creates the expansion for the setter functions (e.g. %SET-FOO-A)
;;; It recursively goes through the slots list and sets up a setter function for each slot
;;; (unless it is read-only). These are not intended for the end user.  These are later
;;; DEFSETF'd in the function EXPAND-REGULAR-DEFSETF so that SETF can be used.
(defun expand-regular-setters (struct-name slots)
  (when slots
    (let* ((slot (first slots))
           (slot-name (first slot))
           (fun-name (intern (concatenate 'string (symbol-name struct-name) "-" (symbol-name slot-name))))
           (read-only (system::%get-plist slot :read-only))
           (slot-type (system::%get-plist slot :type)))
      (cons (unless read-only
              `(defun (setf ,fun-name) (val struct-instance)
                 (declare (type ,slot-type val))
                 (system::%set-defstruct-slot struct-instance ',slot-name val)
                 val))
            (expand-regular-setters struct-name (rest slots))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List expansions (:type list)                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Master function to build all the expansions required for a list-based
;;; struct. It calls various helper functions which build specific parts
;;; of the expansion (i.e. the constructors, accessors, etc.)
(defun expand-list (struct-name java-name options slots-list)
  (let* ((x 1) ;; a hack to patch over a compiler bug
         (constructors (get-constructors options))
         (init-forms (get-init-forms slots-list))
         (named-list (list struct-name))
         (included-struct-name (first (get-included-struct options)))
         (included-slots-list nil)
         (included-init-forms nil))

    (system::%set-defstruct-slot-list struct-name slots-list)
    (system::%set-defstruct-named-list named-list)
    (when included-struct-name
      ;(when (subtypep included-struct-name struct-name)
      ;  (error "~S : Cannot include the structure type itself or one of its subtypes: ~S." struct-name included-struct-name))

      (system::%set-defstruct-named-list (append named-list (system::%get-defstruct-named-list included-struct-name)))

      (setq included-slots-list (get-included-slots-list struct-name options included-struct-name))
      (system::%set-defstruct-slot-list struct-name (append included-slots-list slots-list))

      (setq included-init-forms (get-init-forms included-slots-list))

      (setq init-forms (append included-init-forms init-forms))
      (setf slots-list (append included-slots-list slots-list)))
(eval-when (:compile-toplevel)
(print "the second mapcan.. init-forms: ")(prin1 init-forms)(terpri)
)
    ;; OK, this is a hack. The init-forms may contain constant values. These don't require
    ;; binding and can appear directly as constants in the MAKE- form.
    `(let (,@(mapcan #'(lambda (form) (unless (constantp (second form)) (list form))) init-forms))
       ,@(expand-list-constructor struct-name constructors slots-list options init-forms)
       ,@(expand-shared-readers slots-list options (second (system::%assoc :conc-name options)) (second (system::%assoc :initial-offset options)))
       ,@(expand-shared-predicate (second (system::%assoc :predicate options)) named-list)
       ,@(expand-shared-copier (second (system::%assoc :copier options)))
       ,@(expand-shared-setters struct-name slots-list options 0)
       ',struct-name)))

;;; Builds the expansion for a list constructor. Returns a list with
;;; :initial-offset number of NIL elements preceeding the slot elements.
;;; If the list is named, then the first
(defun expand-list-constructor (struct-name constructors slots-list options init-forms)
  (when constructors
    (let* ((slot-symbols (get-slot-symbols slots-list))
           (slot-values (get-slot-values slots-list))
           (constructor (car constructors))
           (constructor-name (cadr constructor))
           (offset-list (make-list (cadr (system::%assoc :initial-offset options))))
           (constructor-args (or (caddr constructor) `,(get-default-arglist slot-symbols slot-values init-forms))))
      (cons `(defun ,constructor-name ,constructor-args
               ,@(expand-shared-constructor-decls slots-list)
               (if ,(struct-named-p options)
                   (list ,@offset-list ',struct-name ,@(mapcar #'second slot-symbols))
                 (list ,@offset-list ,@(mapcar #'second slot-symbols))))
            (expand-list-constructor struct-name (rest constructors) slots-list options init-forms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vector expansions (:type vector)                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Master function to build all the expansions required for a vector-based
;;; struct. It calls various helper functions which build specific parts
;;; of the expansion (i.e. the constructors, accessors, etc.)
(defun expand-vector (struct-name java-name options slots-list)
  (let* ((x 1) ;; a hack to patch over a compiler bug
         (constructors (get-constructors options))
         (named-list (list struct-name))
         (included-struct-name (first (get-included-struct options)))
         (included-slots-list nil)
         (included-init-forms nil))

    (system::%set-defstruct-slot-list struct-name slots-list)
    (system::%set-defstruct-named-list named-list)
    (when included-struct-name
      ;(when (subtypep included-struct-name struct-name)
      ;  (error "~S : Cannot include the structure type itself or one of its subtypes: ~S." struct-name included-struct-name))

      (system::%set-defstruct-named-list (append named-list (system::%get-defstruct-named-list included-struct-name)))

      (setq included-slots-list (get-included-slots-list struct-name options included-struct-name))
      (system::%set-defstruct-slot-list struct-name (append included-slots-list slots-list))

      (setq included-init-forms (get-init-forms included-slots-list))

      (setq init-forms (append included-init-forms init-forms))
      (setf slots-list (append included-slots-list slots-list)))
(eval-when (:compile-toplevel) 
(print "the third mapcan.. init-forms: ")(prin1 init-forms)(terpri)
)
    ;; OK, this is a hack. The init-forms may contain constant values. These don't require
    ;; binding and can appear directly as constants in the MAKE- form.
    `(let (,@(mapcan #'(lambda (form) (unless (constantp (second form)) (list form))) init-forms))
       ,@(expand-vector-constructor struct-name constructors slots-list options init-forms)
       ,@(expand-shared-readers slots-list options (second (system::%assoc :conc-name options)) (second (system::%assoc :initial-offset options)))
       ,@(expand-shared-predicate (second (system::%assoc :predicate options)) named-list)
       ,@(expand-shared-copier (second (system::%assoc :copier options)))
       ,@(expand-shared-setters struct-name slots-list options 0)
       ',struct-name)))

;;; Builds the expansion for a vector constructor. Returns a vector with
;;; :initial-offset number of NIL elements preceeding the slot elements.
;;; If the vector is named, then the first
(defun expand-vector-constructor (struct-name constructors slots-list options init-forms)
  (when constructors
    (let* ((slot-symbols (get-slot-symbols slots-list))
           (slot-values (get-slot-values slots-list))
           (constructor (car constructors))
           (constructor-name (cadr constructor))
           (offset-list (make-list (cadr (system::%assoc :initial-offset options))))
           (constructor-args (or (caddr constructor) `,(get-default-arglist slot-symbols slot-values init-forms))))
      (cons `(defun ,constructor-name ,constructor-args
               ,@(expand-shared-constructor-decls slots-list)
               (if ,(struct-named-p options)
                   (vector ,@offset-list ',struct-name ,@(mapcar #'second slot-symbols))
                 (vector ,@offset-list ,@(mapcar #'second slot-symbols))))
            (expand-vector-constructor struct-name (rest constructors) slots-list options init-forms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List/Vector shared expansion parts (:type list)/(:type vector) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expand-shared-constructor-decls (slots-list)
  (when slots-list
    (let* ((slot (car slots-list))
           (slot-name (car slot))
           (slot-value (cadr slot))
           (slot-type (system::%get-plist slot :type)))
      ;(when (subtypep (type-of slot-value) slot-type)
      ;  (error "The value ~S does not satisfy the type specifier ~S." slot-value slot-type))
      (cons `(declare (type ,slot-type ,slot-name))
            (expand-shared-constructor-decls (cdr slots-list))))))

;;; This function creates the expansion for the reader functions (e.g. FOO-A, FOO-B, etc.)
;;; It recursively goes through the slots list and sets up an accessor function for each slot.
(defun expand-shared-readers (slots options conc-name offset-value)
  (when slots
    (let* ((slot (car slots))
           (slot-name (car slot)))
      (cons `(defun ,(intern (concatenate 'string (symbol-name conc-name) (symbol-name slot-name))) (struct-instance)
               (if ,(struct-named-p options)
                   (elt struct-instance (+ 1 offset-value (position ',slot-name struct-instance)))
                 (elt struct-instance (+ offset-value (position ',slot-name struct-instance)))))
            (expand-shared-readers (cdr slots) options conc-name offset-value)))))

;;; Builds the expansion for the predicate function (e.g. FOO-P)
(defun expand-shared-predicate (pred-name named-list)
  (when pred-name
    `((defun ,pred-name (struct-instance)
        (member (elt struct-instance 0) named-list)))))

;;; Builds the expansion for the copier function (e.g. COPY-FOO)
(defun expand-shared-copier (copier-name)
  (when copier-name
    `((defun ,copier-name (struct-instance)
        (copy-seq struct-instance)))))

;;; This function creates the expansion for the setter functions (e.g. %SET-FOO-A)
;;; It recursively goes through the slots list and sets up a setter function for each slot
;;; (unless it is read-only). These are not intended for the end user.  These are later
;;; DEFSETF'd in the function EXPAND-VECTOR-DEFSETF so that SETF can be used.
(defun expand-shared-setters (struct-name slots options position-counter)
  (when slots
    (let* ((slot (first slots))
           (slot-name (first slot))
           (fun-name (intern (concatenate 'string (symbol-name struct-name) "-" (string slot-name))))
           (read-only (system::%get-plist slot :read-only))
           (slot-type (system::%get-plist slot :type))
           (offset-value (second (system::%assoc :initial-offset options))))
      (cons (unless read-only
              `(defun (setf ,fun-name) (val struct-instance)
                 (declare (type ,slot-type val))
                 (if ,(struct-named-p options)
                     (setf (elt struct-instance (+ 1 offset-value position-counter)) val)
                   (setf (elt struct-instance (+ offset-value position-counter)) val))
                 val))
            (expand-shared-setters struct-name (rest slots) options (+ position-counter 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLOTS PROCESSING SECTION                                                       ;;;
;;; This section of code performs various error checking and other processing      ;;;
;;; to ensure the slots list is valid and appropriate default values are provided. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns the doc-string from the slots or nil if none exists
(defun get-doc-string (slots)
  (dolist (current slots)
    (when (typep current 'string)
      (return current))))

;;; Dispatcher function which performs various duties regarding
;;; the slots list.  There is error checking and filling in any
;;; missing options-value pairs with default values
(defun process-slots-list (struct-name slots)
  (check-slots-list struct-name slots nil)
  (fill-slots-list slots))

;;; Checks that the slots list provided by the caller is valid.
;;; BNF grammar mandates that slot list has form of just the slot name
;;; or an even numbered keyword/value pair list with the first pair
;;; being slot name and its init form; remaining pairs are slot options
;;; and their values.
(defun check-slots-list (struct-name slots duplicate-checker)
  (let* ((slot (if (listp (car slots)) (car slots) (list (car slots))))
         (slot-name (car slot))
         (new-dc (append duplicate-checker (list slot-name)))
         (count (length slot)))
    (when (member slot-name duplicate-checker)
      (error "~S : Duplicate slots ~S provided." struct-name slot-name))
    (when (and (not (evenp count)) (not (eql count 1)))
      (error "~S : Error in slot descriptions: odd number of elements in keyword/value list." struct-name))
    (when (cdr slots) (check-slots-list struct-name (cdr slots) new-dc))))

;;; Returns an list of slots with any missing options and
;;; their default values filled in
(defun fill-slots-list (slots)
  (when slots
    (let* ((slot (if (listp (car slots)) (car slots) (list (car slots))))
           (slot-name (car slot))
           (slot-init-value (second slot))
           (slot-type (system::%get-plist slot :type t))
           (slot-read-only (system::%get-plist slot :read-only nil))
           (slot-keywordified (intern (symbol-name slot-name) "KEYWORD")))
      (cons (list slot-name slot-init-value :type slot-type :read-only slot-read-only)
            (fill-slots-list (cdr slots))))))

;;; Returns a list of pairs of a slot name and its init form
;;; It takes the slot name and makes a GENTEMP from its name.
;;; These are then used in creating the init forms at runtime.
(defun get-init-forms (slots-list)
  (mapcar #'(lambda (slot)
              (let ((init-form (second slot)))
                (list (gentemp (symbol-name (first slot)) *package*)
                    (if (eq init-form system::%java-null)
                      init-form
                      ;;(progn (print "init-form-to-null: ")(prin1 init-form) )
                      (typecase init-form
                        (null init-form)
                        (number init-form)
                        (character init-form)
                        (string init-form)
                        ;(symbol init-form)
                        ;(otherwise `(function (lambda () ,init-form)))  ;;;NOTE: THIS IS A HACK TO GET AROUND A PROBLEM WHEN USING OTHERWISE IN TYPECASE
                        (t `(function (lambda () ,init-form))))))))
          slots-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GETTERS SECTION                                                                ;;;
;;; This section of code holds several "getters" which return the values of        ;;;
;;; options needed to build the expansions.                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a list of lists which each one indicating the slot name.
(defun get-defstruct-slot-names (slots)
  (mapcar #'(lambda (slot) (car slot)) slots))

;;; By the time this is called, there will be one or more constructors
;;; in the options list (a default one none were supplied).  If one was supplied
;;; with the explicit value NIL, this function will return NIL (indicating
;;; that no constructor should be created). Otherwise, this will return
;;; a list of the constructors.
(defun get-constructors (options &optional (nil-count 0))
  (let ((option (car options)))
    (when (not (cadr (system::%assoc :constructor options)))
      (return-from get-constructors nil))
    (when options
      (if (eq (car option) :constructor)
          (cons option (get-constructors (cdr options)))
        (get-constructors (cdr options))))))

;;; Returns a list of just the slot symbols (without init-form, options etc.).
;;; This is needed for the constructor expansion (which is handled in the
;;; function EXPAND-CONSTRUCTORS)
(defun get-slot-symbols (slots)
  (mapcar #'(lambda (slot) (list (intern (symbol-name (car slot)) "KEYWORD") (car slot))) slots))

;;; Returns a list of just the slot symbols' init values (not slot-name, options
;;; etc.).  This is needed for the constructor expansion (which is handled in
;;; the function EXPAND-CONSTRUCTORS)
(defun get-slot-values (slots)
  (mapcar #'(lambda (slot) (cadr slot)) slots))

;;; Builds and returns the default arglist created from the
;;; slot symbols and the initial values (if any). The heavy
;;; work is actually done by GET-DEFAULT-ARGLIST-AUX.
(defun get-default-arglist (slot-symbols slot-values init-forms)
  (cons '&key (get-default-arglist-aux slot-symbols slot-values init-forms)))

;;; Performs the heavy work for GET-DEFAULT-ARGLIST. This
;;; function returns a normal arglist to be used as the
;;; default arglist for the default constructor of a struct.
(defun get-default-arglist-aux (slot-symbols slot-values init-forms)
  (when slot-symbols
    (let ((maybe-slot-value (first slot-values)))
      (unless (constantp maybe-slot-value)
        (setq maybe-slot-value `(funcall ,(caar init-forms))))
      (cons `(,(first slot-symbols) ,maybe-slot-value)
            (get-default-arglist-aux (rest slot-symbols) (rest slot-values) (rest init-forms))))))

;;; Returns the value of the :include option, or nil if
;;; there is no included struct
(defun get-included-struct (options)
  (let ((included-struct (second (system::%assoc :include options))))
    (if included-struct
        (list included-struct)
      nil)))

;;; Returns the value of the updated included slots, or nil if
;;; there are no updated slots found
(defun get-included-slots-list (struct-name options included-struct-name)
  (let ((x 1) ;; a hack to patch over a compiler bug
        (original-slots (system::%get-defstruct-slot-list included-struct-name))
        (updated-slots (rest (rest (system::%assoc :include options))))
        (current-updated-slot nil)
        (init-value-option nil)
        (type-option t)
        (read-only-option nil)
        (full-included-slots nil))
    (dolist (current-original-slot original-slots full-included-slots)
      (setq current-updated-slot (dolist (slot updated-slots)
                                   (etypecase slot
                                     (list
                                      (if (equal (car slot) (car current-original-slot))
                                          (return slot)))
                                     (symbol
                                      (if (equal slot (car current-original-slot))
                                          (return slot))))))
      (when current-updated-slot
        (etypecase current-updated-slot
          (list
           (when (third current-updated-slot)
             (if (not (or (eql (third current-updated-slot) :type)
                          (eql (third current-updated-slot) :read-only)))
                 (error "~S : Bad option in included slot spec: ~S." struct-name (third current-updated-slot))))
           (setf init-value-option (second current-updated-slot))
           (setf type-option (system::%get-plist current-updated-slot :type))
           (setf read-only-option (system::%get-plist current-updated-slot :read-only nil))

           (when init-value-option
             (setf (second current-original-slot) init-value-option))
           (when type-option
             (setf (fourth current-original-slot) type-option))
           (when (and read-only-option (not (sixth current-original-slot)))
             (setf (sixth current-original-slot) read-only-option)))
          (symbol
           (setf init-value-option nil)
           (setf type-option t)
           (setf read-only-option nil)

           (when init-value-option
             (setf (second current-original-slot) init-value-option))
           (when type-option
             (setf (fourth current-original-slot) type-option))
           (when (not (sixth current-original-slot))
             (setf (sixth current-original-slot) read-only-option)))))

      (setf full-included-slots (append full-included-slots (list current-original-slot))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFAULT SETTINGS SECTION                                                       ;;;
;;; This section of code deals with setting default values for options where no    ;;;
;;; other values were specified.  The dispatcher function SET-OPTION-VALUES calls  ;;;
;;; various other functions to properly set default values.                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dispatcher for functions which set any options values that are
;;; still needed (e.g. default values)
(defun set-option-values (struct-name options)
  (set-print-option-value options)
  (set-copier-value struct-name options)
  (set-initial-offset-value options)
  (set-conc-name-value struct-name options)
  (set-predicate-value struct-name options)
  (set-constructor-value struct-name options)
  ; Nothing is done for :INCLUDE as the default value is no value
  options)

;;; Sets the default constructor name if not explicitly supplied.
(defun set-constructor-value (struct-name options)
  (let ((constructor (system::%assoc :constructor options)))
    (unless (cdr constructor)
      (setf (cdr constructor) (list (intern (concatenate 'string "MAKE-" (symbol-name struct-name)) *package*))))))

;;; Sets the default value for :CONC-NAME which determines the
;;; name to be prefixed to the reader functions
(defun set-conc-name-value (struct-name options)
  (let ((conc-name (system::%assoc :conc-name options)))
    (unless conc-name
      (setf (cdr (last options)) (list `(:conc-name ,(intern (concatenate 'string (symbol-name struct-name) "-" ) *package*)))))))

;;; Sets the name of the structure's predicate function if not
;;; explicitly specified.
(defun set-predicate-value (struct-name options)
  (let ((predicate (system::%assoc :predicate options)))
    (unless (cdr predicate)
      (setf (cdr predicate) (list (intern (concatenate 'string (symbol-name struct-name) "-P") *package*))))))

;;; Adds an internal option, :PRINT-OPTION, whose value is a list of what print-options
;;; were supplied, or nil if none. :PRINT-OPTION is not a valid keyword to be supplied
;;; by the user.  The user supplies either :PRINT-OBJECT XOR :PRINT-FUNCTION and we keep track
;;; of which one it is and its args internally with :PRINT-OPTION.
(defun set-print-option-value (options)
  (let* ((po (system::%assoc :print-object options))
         (pf (system::%assoc :print-function options)))
    (unless (or po pf)
      (setf (cdr (last options)) (list '(:print-option nil))))))

;;; Sets the default name for the copier function if not explicitly supplied
(defun set-copier-value (struct-name options)
  (let ((copier (system::%assoc :copier options)))
    (unless (cdr copier)
      (setf (cdr copier) (list (intern (concatenate 'string "COPY-" (symbol-name struct-name)) *package*))))))

;;; Sets the default value for :INITIAL-OFFSET if not explicitly supplied
;;; If a value has been supplied, it is already guaranteed to be valid
;;; due to earlier checking.  Otherwise, set it to zero.
(defun set-initial-offset-value (options)
  (let ((initial-offset (system::%assoc :initial-offset options)))
    (unless (cadr initial-offset)
      (setf (cdr initial-offset) (list 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LIST PROCESSING SECTION                                                            ;;;
;;; This section of code deals with processing the options list of a defstruct         ;;;
;;; definition.  The dispatcher function PROCESS-OPTIONS-LIST calls various other      ;;;
;;; function to check for errors, listify any naked options, and fill in any           ;;;
;;; options keywords that were not explicitly specified (except for a few exceptions   ;;;
;;; which are mentioned in the comments of the defvar *defstruct-fill-options* form).  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Error checking dispatcher.  Also builds an a-list of valid options.
;;; The order in which these are called may be important (e.g. checking
;;; for invalid naked symbols must be done before the keywords are listified
;;; and filling in missing options should be the last thing done).
(defun process-options-list (struct-name options)
  (check-naked-named-option struct-name options)
  (check-naked-options struct-name options)
  (setf options (listify-options options))
  (check-include-option struct-name options)
  (check-predicate-option struct-name options)
  (check-initial-offset-option struct-name options)
  (check-for-invalid-options struct-name options)
  (check-printer-options struct-name options)
  (check-type-option struct-name options)
  (setf options (fill-missing-options options))
  (check-constructor-options struct-name options)
  options)

;;; Checks that :NAMED is a naked symbol (the only one that MUST be naked).
;;; Errors out or returns NIL
(defun check-naked-named-option (struct-name options)
  (let ((opt nil))
    (when options
      (setf opt (car options))
      (when (and (listp opt) (eql (car opt) :named))
        (error "~S : The :NAMED option cannot be a list." struct-name))
      (check-naked-named-option struct-name (cdr options)))))

;;; Checks for naked keywords where BNF grammar mandates they be in a list
;;; Either errors out or returns NIL
(defun check-naked-options (struct-name options)
  ;; should be only mapc, but there is a compiler problem
  (mapcar #'(lambda (key)
            (when (cl::quick-member key options)
              (error "~S : ~S must be a list." struct-name key)))
        *non-naked-option-keys*)
  nil)

;;; Test for naked symbols and if found put them in a list
(defun listify-options (options)
  (mapcar #'(lambda (x) (if (symbolp x) `(,x) x)) options))

;;; Checks that the :include options, if supplied, is valid.
;;; This merely checks that a value has been supplied per BNF rules.
;;; It does NOT check, for example, that the value of :INCLUDE actually
;;; represents a valid structure.
(defun check-include-option (struct-name options)
  (let ((include (system::%assoc :include options)))
    (when include
      (when (not (cadr include))
        (error "~S : :INCLUDE option must have an argument." struct-name)))))

;;; It is an error if a value for :PREDICATE is supplied when the
;;; struct is unnamed (see function STRUCT-NAMED-P for comments
;;; further explaining this).
(defun check-predicate-option (struct-name options)
  (let ((predicate (system::%assoc :predicate options)))
    (when (and (cadr predicate) (not (struct-named-p options)))
      (error "~S : Cannot create predicate for unnamed structure." struct-name))))

;;; Checks for valid :INITIAL-OFFSET values.
;;; Errors out or returns NIL
(defun check-initial-offset-option (struct-name options)
  (let ((initial-offset (system::%assoc :initial-offset options)))
    (when initial-offset
      (when (not (type-supplied-p options))
        (error "~S : :INITIAL-OFFSET not valid without :TYPE." struct-name))
      (when (not (integerp (cadr initial-offset)))
        (error "~S : :INITIAL-OFFSET value must be an integer." struct-name)))))

;;; Checks for options that are not recognized at all as being valid.
;;; Errors out or returns NIL
(defun check-for-invalid-options (struct-name options)
  (let ((opt (caar options)))
    (when opt
      (unless (cl::quick-member opt *defstruct-valid-options*)
        (error "~S : ~S is not a valid defstruct option." struct-name opt))
      (check-for-invalid-options struct-name (cdr options)))))

;;; Verifies that any supplied printer options are valid
(defun check-printer-options (struct-name options)
  (when (and (system::%assoc :print-object options) (system::%assoc :print-function options))
    (error "~S : :PRINT-OBJECT and :PRINT-FUNCTION cannot be used simultaneously." struct-name))
  (when (and (type-supplied-p options) (or (system::%assoc :print-object options) (system::%assoc :print-function options)))
    (error "~S : Printing options cannot be supplied with :TYPE option." struct-name)))

;;; Checks that :TYPE option is supplied with a valid arg. Errors out
;;; or returns NIL.
(defun check-type-option (struct-name options)
  (let ((type-opt (system::%assoc :type options)))
    (when type-opt
      (when (not (or (eql 'list (cadr type-opt)) (eql 'vector (cadr type-opt))))
        (error "~S : :TYPE must be list or vector." struct-name)))))

;;; does the work for FILL-OPTION-KEYWORDS
(defun fill-alist (alist keys)
  (dolist (key keys alist)
    (unless (system::%assoc key alist)
      (setf alist (cons (list key) alist)))))

;;; Returns list with any missing options filled in
(defun fill-missing-options (options)
  (let* ((options (fill-alist options *defstruct-fill-options*)))
    options))

;;; It is an error if (:constructor nil) is specified along with
;;; any other constructors. If that is this case, this function
;;; will error out. Otherwise it returns NIL.
(defun check-constructor-options (struct-name options)
  (when (and (nil-constructor-p options) (non-nil-constructor-p options))
    (error "~S : Cannot specify (:CONSTRUCTOR NIL) and other constructors at same time." struct-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PREDICATE SECTION                                                                  ;;;
;;; This section of code holds various predicates used by the rest of the defstruct    ;;;
;;; code.                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a generalized boolean. Returns NIL when :TYPE option not supplied.
;;; Otherwise returns T.
(defun type-supplied-p (options)
  (when options
    (when (system::%assoc :type options)
      (return-from type-supplied-p t))))

;;; Returns a generalized boolean. Returns NIL when :NAMED option not supplied.
;;; Otherwise returns T.
(defun named-supplied-p (options)
  (when options
    (when (system::%assoc :named options)
      (return-from named-supplied-p t))))

;;; Returns a generalized boolean. Returns NIL when struct is not named, else returns T.
;;; All structs are named (they are their own type) unless the :TYPE option is used
;;; without the :NAMED option.  In this case they would have whatever type indicated by :TYPE
;;; (e.g. list) and are not a unique type.
(defun struct-named-p (options)
  (if (and (type-supplied-p options) (not (named-supplied-p options)))
      (return-from struct-named-p nil)
    (return-from struct-named-p t)))

;;; Returns a generalized boolean indicating whether or not (:constructor nil)
;;; is in the options list
(defun nil-constructor-p (options)
  (let ((opt (car options)))
    (when opt
      (if (and (eq (car opt) :constructor) (eql (cadr opt) nil))
          (return-from nil-constructor-p t)
        (nil-constructor-p (cdr options))))))

;;; Returns a generalized boolean indicating whether or not any constructor
;;; options have been specified (excluding (:constructor nil)).
(defun non-nil-constructor-p (options)
  (let ((opt (car options)))
    (when opt
      (if (and (eq (car opt) :constructor) (not (null (cadr opt))))
          (return-from non-nil-constructor-p t)
        (non-nil-constructor-p (cdr options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISC SECTION                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Provides a guaranteed unique internal name for the Java defstruct facility to use
(defun javafy-struct-name (struct-name)
  ;;;(symbol-name (gensym (concatenate 'string (symbol-name struct-name) "_" (princ-to-string (get-internal-real-time)))))
  (symbol-name (gensym (concatenate 'string "Defstruct_" (princ-to-string (get-internal-real-time))))))

;; Make this part of the common lisp package
(defun listify-slots (struct)
  (let ((parent (system::%get-defstruct-parent struct)))
    (if parent
        (append (listify-slots (system::%get-defstruct-parent struct)) (system::%get-defstruct-slots struct))
      (system::%get-defstruct-slots struct))))

(defun default-structure-class-printer (struct &optional (stream t))
  (let ((slots (listify-slots struct)))
    (princ "#S" stream)
    (princ (cons (type-of struct)
                 (mapcan #'(lambda (slot)
                             (list (intern (concatenate 'string ":" (symbol-name slot)))
                                   (system::%get-defstruct-slot struct slot)))
                         slots))
           stream)))

(defun sharpsign-S (struct-list)
  (let* ((struct-name (car struct-list))
         (struct-arg-list (rest struct-list))
         (constructor-name (system::%get-defstruct-default-constructor struct-name)))
    `(,constructor-name ,@struct-arg-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COPY-STRUCTURE                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP")

(eval-when (:load-toplevel :execute)
(export '(copy-structure)
        (find-package "COMMON-LISP"))
) ;eval-when

(defun copy-structure (structure)
  (declare (system::%java-class-name "lisp.common.function.CopyStructure"))
  (system::%copy-struct structure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :execute)
(provide "DefstructLsp")
(require "FormatLisp")
) ;eval-when
