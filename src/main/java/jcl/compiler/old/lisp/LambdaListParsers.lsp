;;;; Copyright (c) 2007-2011 College of Charleston, Charleston, SC, USA

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

;;; TODO - implement parsing and generation for macros and destructuring-bind
;;; TODO - create edit for checking that there are no other keys

;;; New lambda-list parser
;;; Based on the mid-term exam in CSCI 325, it's a recursive descent parser. It returns a
;;; data structure that will be attached to the resulting function. The compiler will
;;; then use it to remap the actual args to the BOA form (funcall method).
;;;
;;; Here's the LL(1) grammar for the parse.
#|
	ORDINARY-COMPILER    -> ( REQUIRED-FORM OPTIONAL-FORM REST-KEY-FORMS AUX-FORM)
        MACRO-COMPILER       -> ( WHOLE-FORM ENV-FORM MACRO-REQUIRED-FORM ENV-FORM MACRO-OPTIONAL-FORM
                                  ENV-FORM REST-BODY-KEY-FORMS ENV-FORM AUX-FORM ENV-FORM )
        MACRO-COMPILER       -> ( WHOLE-FORM ENV-FORM MACRO-REQUIRED-FORM ENV-FORM MACRO-OPTIONAL-FORM
                                  ENV-FORM . REST-VAR )
	SPECIALIZED-COMPILER -> ( SPECIALIZED-REQUIRED-FORM OPTIONAL-FORM REST-FORM KEY-FORM AUX-FORM)
        GENERIC-FN-COMPILER  -> ( REQUIRED-FORM GENERIC-OPTIONAL-FORM REST-FORM GENERIC-KEY-FORM)
	BOA-COMPILER         ->   ORDINARY-COMPILER
	DEFSETF-COMPILER     -> ( REQUIRED-FORM OPTIONAL-FORM REST-FORM KEY-FORM ENV-FORM)
        DEFTYPE-COMPILER     -> ( WHOLE-FORM ENV-FORM REQUIRED-FORM ENV-FORM KEY-FORM ENV-FORM AUX-FORM ENV-FORM )
        DEFINE-MODIFY-MACRO-COMPILER       -> ( REQUIRED-FORM DMM-OPTIONAL-FORM REST-FORM )
        DEFINE-METHOD-COMBINATION-COMPILER -> ( WHOLE-FORM REQUIRED-FORM OPTIONAL-FORM REST-FORM KEY-FORM AUX-FORM)

        ;;; All of these mumble-VAR forms are a handy way to pass the correct value
        ;;; to the VAR handler, eg (var form :required)
        REQUIRED-VAR   -> VAR
        OPTIONAL-VAR   -> VAR
        SUPPLIED-P-VAR -> VAR
        REST-VAR       -> VAR
        KEY-VAR        -> VAR
        AUX-VAR        -> VAR

	VAR -> symbol

        INIT-FORM -> form

	SUPPLIED-P -> SUPPLIED-P-VAR
        SUPPLIED-P -> e // this is needed to make it LL(1)

	REQUIRED-FORM -> REQUIRED-VARS
        REQUIRED-FORM -> e

	REQUIRED-VARS -> REQUIRED-VAR REQUIRED-VARS
        REQUIRED-VARS -> e

	OPTIONAL-FORM -> &optional OPTIONS
        OPTIONAL-FORM -> e

        REST-KEY-FORMS -> REST-FORM KEY-FORM
        REST-KEY-FORMS -> FAKE-REST-FORM KEY-FORM
        REST-KEY-FORMS -> e

	REST-FORM -> &rest REST-VAR
        FAKE-REST-FORM -> e

	OPTIONS -> OPTION OPTIONS
        OPTIONS -> e

	OPTION -> EXTENDED-OPTION
        OPTION -> OPTION-VAR

	EXTENDED-OPTION -> ( OPTION-VAR EXTENDED-INIT )

        EXTENDED-INIT -> INIT-FORM SUPPLIED-P-VAR
        EXTENDED-INIT -> INIT-FORM
        EXTENDED-INIT -> e

        KEY-FORM -> &key KEYS ALLOW-OTHER-KEYS

        KEYS -> KEY KEYS
        KEYS -> e

	KEY -> EXTENDED-KEY
        KEY -> KEY-VAR

	EXTENDED-KEY -> ( KEY-FULL-VAR EXTENDED-INIT )

	KEY-FULL-VAR -> ( EXTENDED-KEY-VAR )
        KEY-FULL-VAR -> KEY-VAR

	EXTENDED-KEY-VAR -> KEYWORD-NAME KEY-VAR
        KEYWORD-NAME     -> KEYWORD-NAME-VAR

	ALLOW-OTHER-KEYS -> &allow-other-keys
	ALLOW-OTHER-KEYS -> e

	AUX-FORM -> &aux AUX-VARS

	AUX-VARS -> AUX-FULL-VAR AUX-VARS
        AUX-VARS -> e

	AUX-FULL-VAR -> EXTENDED-AUX-VAR
        AUX-FULL-VAR -> AUX-VAR
        AUX-FULL-VAR -> e

	EXTENDED-AUX-VAR -> ( AUX-VAR AUX-INIT )

	AUX-INIT -> INIT-FORM
        AUX-INIT -> e

        ;;; Now we add the non-terminals for macro lambda parsing

        WHOLE-VAR -> VAR
        ENV-VAR   -> VAR
        BODY-VAR  -> VAR

        WHOLE-FORM -> &whole WHOLE-VAR
        WHOLE-FORM -> e

        ENV-FORM   -> &environment ENV-VAR
        ENV-FORM   -> e

        MACRO-REQUIRED-FORM -> MACRO-REQUIRED-VARS
        MACRO-REQUIRED-FORM -> e

        MACRO-REQUIRED-VARS -> MACRO-REQUIRED-VAR MACRO-REQUIRED-VARS
        MACRO-REQUIRED-VARS -> e

        MACRO-REQUIRED-VAR -> DESTRUCTURING-FORM
        MACRO-REQUIRED-VAR -> REQUIRED-VAR

        DESTRUCTURING-FORM -> ( WHOLE-FORM MACRO-REQUIRED-FORM MACRO-OPTIONAL-FORM REST-BODY-KEY-FORMS )
        DESTRUCTURING-FORM -> ( WHOLE-FORM MACRO-REQUIRED-FORM MACRO-OPTIONAL-FORM . REST-VAR )

	MACRO-OPTIONAL-FORM -> &optional MACRO-OPTIONS
        MACRO-OPTIONAL-FORM -> e

	MACRO-OPTIONS -> MACRO-OPTION MACRO-OPTIONS
        MACRO-OPTIONS -> e

	MACRO-OPTION -> MACRO-EXTENDED-OPTION
        MACRO-OPTION -> OPTION-VAR

	MACRO-EXTENDED-OPTION -> ( DESTRUCTURING-FORM EXTENDED-INIT )
	MACRO-EXTENDED-OPTION -> ( OPTION-VAR EXTENDED-INIT )

        REST-BODY-KEY-FORMS -> REST-BODY-FORM ENV-FORM MACRO-KEY-FORM
        REST-BODY-KEY-FORMS -> FAKE-REST-FORM MACRO-KEY-FORM
        REST-BODY-KEY-FORMS -> e

	REST-BODY-FORM -> &rest REST-VAR
	REST-BODY-FORM -> &body BODY-VAR
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
The structure of the parsed lambda lists. The basic structure is an association
that follows the major section keys. The CDR of an entry in the assoc list
is a property list with the details of the parameter.

The possible properties vary with the usage of the parameter (:REQUIRED, :KEY, etc).
The basic properties common to all usages are:
    :SCOPE (values :lexical or :dynamic)
    :TYPE  (defaulted to T but updated by the compiler)
    :USAGE (:REQUIRED, :OPTIONAL, :REST, :KEY, :AUX)

    :OPTIONAL and :KEY add
        :INIT-FORM (a lisp form that will be wrapped in a lambda and called to get the initial value)
        :SUPPLIED-P (a variable whose value is a generalized boolean)

    :KEY further adds
        :KEYWORD-NAME
        :ALLOW-OTHER-KEYS (this is found in an entry with the name &allow-other-keys, usage :ALLOW-OTHER-KEYS and no index)

    :AUX adds
        :INIT-FORM (same as for optional and key but no supplied-p as well)

For example for the lambda list (a b &key &allow-other-keys), the parse would be
((a :type t :usage :required ...) (b :type t :usage :required ....) (&allow-other-keys :usage :ALLOW-OTHER-KEYS))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "COMMON-LISP")

(eval-when (:compile-toplevel :load-toplevel :execute)
(require "CoreFunctionsLsp")
) ;eval-when

(in-package "COMPILER")
(use-package '(COMMON-LISP) "COMPILER")

(eval-when (:load-toplevel :execute)
(export
  (list 'parse-ordinary-lambda-list 'generate-arglist-analyzer 'provide-init-form-usage)
  (find-package "COMPILER"))
) ;eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; useful utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simple-constantp (form)
  (cond ((eq form t) t)
        ((eq form nil) t)
        ((eq form 'pi) t)
        ((eq form 'e) t)
        ((eq form system::%java-null) t)
        ((numberp form) t)
        ((arrayp form) t)
        ((characterp form) t)
        ((keywordp form) t)
        ((and (consp form) (eq (car form) 'quote)) t)
        ((functionp form) t)
        (t nil)))

(defun lambda-param-symbol-p (form)
  (declare (system::%java-class-name "lisp.system.compiler.LambdaParamSymbolP"))
  (and (symbolp form)
       (not (common-lisp::quick-member form lambda-list-keywords))))

(defun keywordify-symbol (symbol)
  (declare (system::%java-class-name "lisp.system.compiler.KeywordifySymbol"))
  (intern (symbol-name symbol) "KEYWORD"))

(eval-when (:compile-toplevel :load-toplevel :execute)
(setq *init-form-param-list* nil) (system::%set-special '*init-form-param-list* t)
(setq *parameter-index* 0)        (system::%set-special '*parameter-index* t)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some productions common to the other grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-> Everything (mostly) goes through here eventually
(defun var (var usage)
  (declare (system::%java-class-name "lisp.system.compiler.Var"))
  (if (symbolp var)
    (progn
      (setq *init-form-param-list* (append *init-form-param-list* (list var)))
      `(,var :scope :lexical
              :allocation (:parameter . ,(setq *parameter-index* (1+ *parameter-index*)))
              :usage ,usage
              :type
              ,(if (or (eq usage :rest)
                       (eq usage :body)
                       (eq usage :whole))
               'list
               (if (eq usage :supplied-p)
                 'boolean
                 t))))
    (error "Expected a symbol in ~a: ~S" usage var)))

(defun whole-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.WholeVar"))
  (var form :whole))

(defun environment-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.EnvironmentVar"))
  (var form :environment))

(defun required-var (form)
  ;; don't ask. Just let it be there...
  ;(declare (system::%java-class-name "lisp.system.compiler.RequiredVar"))
  (var form :required))

(defun optional-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.OptionalVar"))
  (var form :optional))

(defun supplied-p-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.SuppliedPVar"))
  (var form :supplied-p))

(defun tag-param-has-supplied-indicator (form)
  (if (symbolp form)
    `(:has-supplied-p ,form)
    (error "Expected a symbol in ~a: ~S" :supplied-p form)))

(defun rest-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.RestVar"))
  (let* ((retval (var form :rest))
         (lcl (rest retval)))
    (setq lcl (cons :init-form (cons nil lcl)))
    (cons (first retval) lcl)))

(defun body-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.BodyVar"))
  (let* ((retval (var form :body))
         (lcl (rest retval)))
    (setq lcl (cons :init-form (cons nil lcl)))
    (cons (first retval) lcl)))

(defun key-var (form key-name)
  (declare (system::%java-class-name "lisp.system.compiler.KeyVar"))
  (let* ((retval (var form :key))
         (lcl (rest retval)))
    (setq lcl (cons :key-name (cons key-name lcl)))
    (cons (first retval) lcl)))

(defun destructured-key (form key-name)
  (declare (system::%java-class-name "lisp.system.compiler.DestructuredKey"))
  (let* ((retval (var 'foo :key)) ; dropped later, just gets the form
         (lcl (rest retval)))
    (setq lcl (cons :key-name (cons key-name lcl)))
    (cons (destructuring-form form) lcl)))

(defun allow-other-keys-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.AllowOtherKeysVar"))
  ;; the form will be T if present, nil otherwise
  `(,form :usage :allow-other-keys :type boolean))

(defun aux-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.AuxVar"))
  (var form :aux))

;;-> except for this fake variable that is inserted before the &key params
;;   to provide a "&rest"-like hook to create the list of the rest of the
;;   args for the &key variables to find the proper values
(defun fake-rest-form ()
  (declare (system::%java-class-name "lisp.system.compiler.FakeRestForm"))
  '|FakeRestSymbolForHandlingKeysWithout&Rest|)

;;; Lambdafication of the form happens in creating the inserted initializing code.
(defun init-form (form) form)

;;;
;;; Now let's get into the actual parsers
;;;

;;; --------> Start Ordinary Lambda List <-----------

;;; (a b c)
(defun required-vars (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.RequiredVars"))
  (when form
      (if (lambda-param-symbol-p (first form))
          (cons
            (required-var (first form))
            (required-vars (rest form) fn-list))
        (funcall (first fn-list) form (rest fn-list)))))

;;; (a b c ...)
(defun required-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.RequiredForm"))
  (when form
      (if (lambda-param-symbol-p (first form))
          (required-vars form fn-list)
        (funcall (first fn-list) form (rest fn-list)))))

;;; (&optional d e f ...)
(defun optional-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.OptionalForm"))
  (when form
      (if (eq (first form) '&optional)
        (options (rest form) fn-list)
        (funcall (first fn-list) form (rest fn-list)))))

;;; (d (e 1 p) f)
(defun options (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.Options"))
  (when form
    (if (or (lambda-param-symbol-p (first form))
            (and (first form) (listp (first form))))
      (append
        (option (first form))
        (options (rest form) fn-list))
      (funcall (first fn-list) form (rest fn-list)))))

;;; d, (e 1 p), f
(defun option (form)
  (declare (system::%java-class-name "lisp.system.compiler.Option"))
  (if (lambda-param-symbol-p form)
    (option `(,form))
    (if (listp form)
      (extended-option form)
      (error "Expected a symbol or a list: ~S" form))))

;;; (e 1 p)
(defun extended-option (form)
  (declare (system::%java-class-name "lisp.system.compiler.ExtendedOption"))
  (if (listp form)
      (let ((opt-var (optional-var (first form)))
            (ext-init (extended-init (rest form))))

        ;;***** Hack alert ******
        ;; This handles a lack of smarts in the compiler. If the init form
        ;; is a special variable, it is wrapped to make the system deal with
        ;; it as a special variable.
        (let ((1st-ext-init (first ext-init)))
          (when (and 1st-ext-init (symbolp 1st-ext-init) (not (simple-constantp 1st-ext-init)) (system::%get-special 1st-ext-init))
            (setq ext-init `((symbol-value ',1st-ext-init) ,@(rest ext-init)))))
        ;;***** End Hack Alert *****

        ;; add the init form to the optional
        (setq opt-var (append opt-var `(:init-form ,(first ext-init) ,@(second ext-init))))
        ;; now we have an extended option parse element
        (unless (endp (cdddr form))
          (error "Extraneous information at end of an extended option: ~S" form))
        (if (cadr ext-init)
          (list opt-var (supplied-p-var (cadr (cadr ext-init))))
          (list opt-var)))
    (error "Expected an extended option specification: ~S" form)))

;;; 1, p
(defun extended-init (form)
  (declare (system::%java-class-name "lisp.system.compiler.ExtendedInit"))
  (unless (endp form)
    (list
      (init-form (first form))
      (supplied-p (second form)))))
;;; p
(defun supplied-p (form)
  (declare (system::%java-class-name "lisp.system.compiler.SuppliedP"))
  (when form
    ;; add a note to the param elt that there is a supplied-p coming
    (tag-param-has-supplied-indicator form)))

;;; (&rest r &key... | &key ... | e)
(defun rest-key-forms (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.RestKeyForms"))
  (when form
    (if (eq (first form) '&rest)
      (rest-form form fn-list)
      (funcall (first fn-list) form (rest fn-list)))))

;;; (&rest r)
(defun rest-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.RestForm"))
  (when form
      (if (eq (first form) '&rest)
          (cons
            (rest-var (second form))
            (funcall (first fn-list) (cddr form) (rest fn-list)))
        (funcall (first fn-list) form (rest fn-list)))))

;;; (&key a b c ...)
;;;   -> we also remember that we saw an &key even there are no keys
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*&key-found* t)
(setq *&key-found* nil)
); eval-when

(defun key-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.KeyForm"))
  (when form
    (if (eq (first form) '&key)
        (let ((*&key-found* t))
          (keys (rest form) fn-list))  ;; handles 1 & 2
#|
          ;; 1. This usual form. Has &key and pairs of parameters. May have &allow-other-keys
          (keys (rest form) fn-list) ;; will handle keys and adds &allow-other-keys in parse
            ;; 2. &key &allow-other-keys ;; same, just no keys
              ;; 3. &keys with nothing or &aux ;; keys with nothing needs :allow-other-keys in parse
                ;; 4. &allow-other-keys - error
|#
        (funcall (first fn-list) form (rest fn-list)))))

;;; (a b c ...)
(defun keys (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.Keys"))
  (when form
    (if (or (lambda-param-symbol-p (first form))
            (and (first form) (listp (first form))))
      (append
        (key (first form))
        (keys (rest form) fn-list))
      (funcall (first fn-list) form (rest fn-list)))))

;;; a or (a ...)
(defun key (form)
  (declare (system::%java-class-name "lisp.system.compiler.Key"))
  (if (lambda-param-symbol-p form)
      (key `(,form))
    (if (listp form)
      (extended-key form)
      (error "Expected an extended key specification: ~S" form))))

;;; (a ...) or ((a x) ...)
(defun extended-key (form)
  (declare (system::%java-class-name "lisp.system.compiler.ExtendedKey"))
  (if (listp form)
      (let ((the-key-var (key-full-var (first form)))
            (ext-init (extended-init (rest form))))

        ;;***** Hack alert ******
        ;; This handles a lack of smarts in the compiler. If the init form
        ;; is a special variable, it is wrapped to make the system deal with
        ;; it as a special variable.
        (let ((1st-ext-init (first ext-init)))
          (when (and 1st-ext-init (symbolp 1st-ext-init) (not (simple-constantp 1st-ext-init)) (system::%get-special 1st-ext-init))
            (setq ext-init `((symbol-value ',1st-ext-init) ,@(rest ext-init)))))
        ;;***** End Hack Alert *****

        ;; add the init form to the optional var
        (setq the-key-var (append the-key-var `(:init-form ,(first ext-init) ,@(second ext-init))))
        (unless (endp (cdddr form))
          (error "Extraneous material in an extended key form: ~S" (first form)))
        (if (cadr ext-init)
          (list the-key-var (supplied-p-var (cadr (cadr ext-init))))
          (list the-key-var)))
    (error "Expected a list for an extended key form: ~S" (first form))))

;;; a or (a x)
(defun key-full-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.KeyFullVar"))
  (if (symbolp form)
      (key-full-var `(,(keywordify-symbol form) ,form))
    (if (listp form)
        (progn
          (unless (endp (cddr form))
            (error "Extra material in extended key variable: ~S" form))
          (extended-key-var form))
      (error "Expected a symbol or a list: ~S" form))))

;;; (a x)
(defun extended-key-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.ExtendedKeyVar"))
  (unless (endp (cddr form))
    (error "Extraneous material in keyword name specification: ~S" (first form)))
  (key-var (second form)
           (or (keyword-name (first form))
               (keywordify-symbol (second form)))))

;;; a
(defun keyword-name (form)
  (declare (system::%java-class-name "lisp.system.compiler.KeywordName"))
  (if (lambda-param-symbol-p form)
      form
    (error "Keyword name parameter is not a symbol: ~S" form)))

;;; ------> This has to be meld into the &key, since you can't have this with that.
;;; (&allow-other-keys ....)
(defun allow-other-keys (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.AllowOtherKeys"))
  (when form
    (if (eq (first form) '&allow-other-keys)
        (progn
          (unless *&key-found* (error "&ALLOW-OTHER-KEYS not followed by an &KEY form: " form))
          (cons
            (allow-other-keys-var '&allow-other-keys)
            (funcall (first fn-list) (rest form) fn-list)))
        (funcall (first fn-list) form (rest fn-list)))))

;;; TODO - let this parser completely parse aux variables, putting them into the
;;;    parsed-lambda-list elements. That makes it easy to pull out the declarations
;;;    later and makes sure that the variable names aren't duplicated.

;;; AUX forms are different from the other lambda list forms. These are
;;; really a specification for additional bindings within the body of the
;;; function. There is no sense in making additional parse information
;;; since it has nothing to do with apportioning arguments to the
;;; lambda list. So the parser will do a sanity check for the specification
;;; (correct syntax and not re-using any parameter names). Then it will a second
;;; value that is the rest of the lambda list after the &aux symbol. The
;;; compiler will create a let* form where the binding list is the second value
;;; and the rest of the let* wraps the body of the function. E.g.
;;;   `(let* (,second-value) ,function-body)
;;;
;;; NOTE: the compiler will have to take care to correctly handles the portion of any
;;; declarations in the function body that pertain to the aux variables. Declarations
;;; pertaining the true parameters must lexically precede the let* form. So it may be
;;; `((declare ,true-parameter-decls)
;;;   (let* (,second-value)
;;;     (declare ,aux-var-decls)
;;;     ,function-body))
;;;
;;; (&aux a b ....
(defun aux-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.AuxForm"))
  (when form
      (if (eq (first form) '&aux)
          (progn
            (aux-vars (rest form) fn-list))
        (error "Extraneous material where an &aux was expected, ~S" form))))

;;; (a b ...)
(defun aux-vars (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.AuxVars"))
  (when form
      (if form
          (cons
            (aux-full-var (first form))
            (aux-vars (rest form) fn-list))
        (funcall (first fn-list) form (rest fn-list)))))

;;; a or (a init)
(defun aux-full-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.AuxFullVar"))
  (if (listp form)
      (extended-aux-var form)
    (if (lambda-param-symbol-p form)
        (extended-aux-var `(,form nil))
      (error "~%Expecting a symbol or an extended aux variable, ~S" form))))

;;; (a init)
(defun extended-aux-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.ExtendedAuxVar"))
  (if (listp form)
      (let ((the-var (aux-var (first form)))
            (the-init (aux-init (second form))))
        (unless (endp (cddr form))
          (error "Extraneous material in extended aux var: ~S" (first form)))
        (append the-var `(:init-form ,the-init)))
    (error "Expected an extended aux var specification: ~S" (first form))))

;;; init
(defun aux-init (form)
  (declare (system::%java-class-name "lisp.system.compiler.AuxInit"))
  (init-form form))

;;; --------> End Ordinary Lambda List <-----------

;;; --------> Start Macro Lambda List  <-----------
;;; (&whole w)
(defun whole-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.WholeForm"))
  (when form
      (if (eq (first form) '&whole)
          (cons
            (whole-var (second form))
            (funcall (first fn-list) (cddr form) (rest fn-list)))
        ;; having a default &whole param turns out to be helpful...
        (cons
          (whole-var (gensym "&WHOLE-"))
          (funcall (first fn-list) form (rest fn-list))))))

;;; (&environment w)
(defun environment-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.EnvironmentForm"))
  (when form
      (if (eq (first form) '&environment)
          (cons
            (environment-var (second form))
            (funcall (first fn-list) (cddr form) (rest fn-list)))
        (funcall (first fn-list) form (rest fn-list)))))

;;; (a b c)
(defun macro-required-vars (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.MacroRequiredVars"))
  (when form
      (if (or (lambda-param-symbol-p (first form))
              (listp (first form)))
          (cons
            (macro-required-var (first form))
            (macro-required-vars (rest form) fn-list))
        (funcall (first fn-list) form (rest fn-list)))))

;;; (a b c ...)
(defun macro-required-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.MacroRequiredForm"))
  (when form
      (if (not (common-lisp::quick-member (first form) lambda-list-keywords))
          (macro-required-vars form fn-list)
        (funcall (first fn-list) form (rest fn-list)))))

;;; a or (x ...)
(defun macro-required-var (form)
  (declare (system::%java-class-name "lisp.system.compiler.MacroRequiredVar"))
  (if (listp form)
    (destructuring-form form)
    (required-var form)))

;;; (&optional d e f ...)
(defun macro-optional-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.MacroOptionalForm"))
  (when form
      (if (eq (first form) '&optional)
          (macro-options (rest form) fn-list)
        (funcall (first fn-list) form (rest fn-list)))))

;;; (d (e 1 p) f)
(defun macro-options (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.MacroOptions"))
  (when form
      (if (or (lambda-param-symbol-p (first form))
              (and (first form) (listp (first form))))
        (let* ((current-option (macro-option (first form)))
               (is-supplied (system::%get-plist (rest (car current-option)) :supplied-p)))
          (append
            current-option
            (when is-supplied
              `((,is-supplied
                 :type boolean :scope :lexical
                 :allocation (:parameter . ,(setq *parameter-index* (1+ *parameter-index*)))
                 :usage :supplied-p)))
            (macro-options (rest form) fn-list)))
        (funcall (first fn-list) form (rest fn-list)))))

;;; d, (e 1 p), f
(defun macro-option (form)
  (declare (system::%java-class-name "lisp.system.compiler.MacroOption"))
  (if (lambda-param-symbol-p form)
    (macro-option `(,form))
    (if (listp form)
      (macro-extended-option form)
      (error "Expected a symbol or a list: ~S" form))))

;;; (e 1 p) --***
(defun macro-extended-option (form)
  (declare (system::%java-class-name "lisp.system.compiler.MacroExtendedOption"))
  (if (listp form)
      (let ((opt-var (optional-var-or-destructuring-form (first form)))
            (ext-init (extended-init (rest form))))

        ;;***** Hack alert ******
        ;; This handles a lack of smarts in the compiler. If the init form
        ;; is a special variable, it is wrapped to make the system deal with
        ;; it as a special variable.
        (let ((1st-ext-init (first ext-init)))
          (when (and 1st-ext-init (symbolp 1st-ext-init) (not (simple-constantp 1st-ext-init)) (system::%get-special 1st-ext-init))
            (setq ext-init `((symbol-value ',1st-ext-init) ,@(rest ext-init)))))
        ;;***** End Hack Alert *****

        ;; add the init form to the optional var
        (setq opt-var
          (append opt-var `(:init-form ,(first ext-init) ,@(second ext-init))))
        (unless (endp (cdddr form))
          (error "Extraneous information at end of an extended option: ~S" form))
        `(,opt-var))
    (error "Expected an extended option specification: ~S" form)))

(defun optional-var-or-destructuring-form (form)
  (declare (system::%java-class-name "lisp.system.compiler.OptionalVarOrDestructuringForm"))
  (if (listp form)
    (destructuring-form form)
    (optional-var form)))

;;; ([&rest | &body] r &key... | &key ... | e)
(defun rest-body-key-forms (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.RestBodyKeyForms"))
  (when form
    (if (eq (first form) '&rest)
      (rest-form form fn-list)
      (if (eq (first form) '&body)
        (body-form form fn-list)
        (funcall (first fn-list) form (rest fn-list))))))

;;; (&body r)
(defun body-form (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.BodyForm"))
  (when form
      (if (eq (first form) '&body)
          (cons
            (body-var (second form))
            (funcall (first fn-list) (cddr form) (rest fn-list)))
        (funcall (first fn-list) form (rest fn-list)))))

;;; --------> End Macro Lambda List  <-----------

;;; --------> Start Destructuring Lambda List  <-----------

;;; Performs a restricted form of macro lambda. Note that this isn't exactly in the
;;; modularity implied from the Hyperspec. The HS folds symbols into the overall
;;; destructuring algorithm. This is only called when the lambda list element is a list
;;; that may contain a restricted macro lambda list (no &environment or &aux parameters).

(defun destructuring-form (form)
  (declare (system::%java-class-name "lisp.system.compiler.DestructuringForm"))
  (if (listp form)
    (parse-lambda-list form *destructuring-lambda-list-parse-fns*)
    (error "A destructuring form must be a restricted macro lambda list, ~S" form)))

;;; --------> End Destructuring Lambda List  <-----------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Validation functions for parsed lambda lists ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-for-multiple-variables (parsed-lambda-list)
  (declare (system::%java-class-name "lisp.system.compiler.CheckForMultipleVariables"))
  (let ((results (check-for-multiple-variables-aux parsed-lambda-list nil)))
    (when results
      (error "Multiple uses of the same variable(s): ~S" results))))

(defun check-for-multiple-variables-aux (parsed-lambda-list current)
  (declare (system::%java-class-name "lisp.system.compiler.CheckForMultipleVariablesAux"))
  (when parsed-lambda-list
    (let ((curr-variable (caar parsed-lambda-list)))
      (if (common-lisp::quick-member curr-variable current)
        (cons curr-variable (check-for-multiple-variables-aux (cdr parsed-lambda-list) current))
        (check-for-multiple-variables-aux (cdr parsed-lambda-list) (cons curr-variable current))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; The external functions for parsing the various lambda lists ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-lambda-list (form fn-list)
  (declare (system::%java-class-name "lisp.system.compiler.ParseLambdaList"))
  ;; used to carry a lambda list for init forms in &optional and &key
  (let ((*init-form-param-list* nil))
    (if (listp form)
        (let* ((result (funcall (first fn-list) form (rest fn-list)))
               (aux-forms (rest (common-lisp::quick-member '&aux form))))
          (check-for-multiple-variables result)
          (values result aux-forms))
      (error "Lambda list must be a list: ~S" form))))
); eval-when

;;-- ORDINARY
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*ordinary-lambda-list-parse-fns* t)
(setq *ordinary-lambda-list-parse-fns*
  (list #'required-form #'optional-form #'rest-key-forms #'key-form #'allow-other-keys #'aux-form))
)

(defun parse-ordinary-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseOrdinaryLambdaList"))
  (setq *parameter-index* 0)
  (parse-lambda-list form *ordinary-lambda-list-parse-fns*))

;;-- MACRO
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*macro-lambda-list-parse-fns* t)
(setq *macro-lambda-list-parse-fns*
  (list #'whole-form #'environment-form #'macro-required-form #'environment-form #'macro-optional-form
        #'environment-form #'rest-body-key-forms #'environment-form #'key-form #'allow-other-keys
        #'environment-form #'aux-form #'environment-form))
)

(defun parse-macro-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseMacroLambdaList"))
  (setq *parameter-index* 1)
  (parse-lambda-list form *macro-lambda-list-parse-fns*))

;;-- DEFSETF
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*defsetf-lambda-list-parse-fns* t)
(setq *defsetf-lambda-list-parse-fns*
  (list #'required-form #'environment-form #'optional-form #'environment-form #'rest-key-forms
        #'environment-form #'key-form #'environment-form #'allow-other-keys #'environment-form))
)

(defun parse-defsetf-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseDefsetfLambdaList"))
  (setq *parameter-index* 0)
  (parse-lambda-list form *defsetf-lambda-list-parse-fns*))

;;-- DESTRUCTURING
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*destructuring-lambda-list-parse-fns* t)
(setq *destructuring-lambda-list-parse-fns*
  (list #'whole-form #'macro-required-form #'macro-optional-form #'rest-body-key-forms #'key-form))
)

(defun parse-destructuring-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseDestructuringLambdaList"))
  (setq *parameter-index* 0)
  (parse-lambda-list form *destructuring-lambda-list-parse-fns*))

;;-- BOA
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*boa-lambda-list-parse-fns* t)
(setq *boa-lambda-list-parse-fns* nil)
)

;;-- DEFINE-METHOD-COMBINATION-ARGUMENTS
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*define-method-combination-arguments-lambda-list-parse-fns* t)
(setq *define-method-combination-arguments-lambda-list-parse-fns*
  (cons #'whole-form *ordinary-lambda-list-parse-fns*))
)

(defun parse-define-method-combination-arguments-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseDefineMethodCombinationArgumentsLambdaList"))
  (setq *parameter-index* 0)
  (parse-lambda-list form *define-method-combination-arguments-lambda-list-parse-fns*))

;;-- GENERIC-FUNCTION
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*generic-function-lambda-list-parse-fns* t)
(setq *generic-function-lambda-list-parse-fns* nil)
)
;;-- SPECIALIZED
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*specialized-lambda-list-parse-fns* t)
(setq *specialized-lambda-list-parse-fns* nil)
)

;;-- DEFTYPE
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*deftype-lambda-list-parse-fns* t)
(setq *deftype-lambda-list-parse-fns*
  (list #'whole-form #'environment-form #'macro-required-form #'environment-form #'macro-optional-form
        #'environment-form #'rest-body-key-forms #'environment-form #'key-form #'allow-other-keys
        #'environment-form #'aux-form #'environment-form))
)

(defun clean-init-forms (parse-list)
  (system::%mapcar #'(lambda (parse-line)
                       (let ((parse-rest (rest parse-line)))
                         (unless (system::%get-plist parse-rest :init-form)
                           (system::%set-plist parse-rest :init-form '*)))) parse-list))

(defun parse-deftype-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseDeftypeLambdaList"))
  (setq *parameter-index* 1)
  (let ((result (parse-lambda-list form *deftype-lambda-list-parse-fns*)))
    (clean-init-forms result)
    result))

;;-- DEFINE-MODIFY-MACRO
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*define-modify-macro-lambda-list-parse-fns* t)
(setq *define-modify-macro-lambda-list-parse-fns*
  (list #'required-form #'optional-form #'rest-key-forms))
)

(defun parse-define-modify-macro-lambda-list (form)
  (declare (system::%java-class-name "lisp.system.compiler.ParseDefineModifyMacroLambdaList"))
  (setq *parameter-index* 0)
  (parse-lambda-list form *define-modify-macro-lambda-list-parse-fns*))
;;==============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; End of Lambda List Parsing Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Code to create a function used by another function to enable APPLY ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-args-for-apply-aux (parsed-lambda-list)
 (system::%mapcar
   #'(lambda (parsed-lambda-list-elt)
       (case (get-usage parsed-lambda-list-elt)
         (:required
           `(let ((the-value
                   (if arglist
                     (first arglist)
                     (error "APPLY error: no value given for required parameter " ',(first parsed-lambda-list-elt)))))
                (setq arglist (rest arglist))
                the-value))
         (:optional
           `(let ((the-value
                    (if arglist
                      (first arglist)
                      ,(system::%get-plist (rest parsed-lambda-list-elt) :init-form))))
              ,(if (get-has-supplied-p parsed-lambda-list-elt)
                `(setq supplied-p-true arglist)
                `(setq supplied-p-true nil))
              (setq arglist (rest arglist))
              the-value))
         (:rest
           (unless (eq (car parsed-lambda-list-elt) (fake-rest-form))
             `arglist))

         (:key
           `(let* ((the-key-name  ',(system::%get-plist (rest parsed-lambda-list-elt) :key-name))
                   (the-init-form ',(system::%get-plist (rest parsed-lambda-list-elt) :init-form))
                   (the-value (system::%get-plist arglist the-key-name the-init-form))
                   (key-not-present-in-arglist (eq the-value the-init-form)))
                (setq supplied-p-true (not key-not-present-in-arglist))
              the-value))
         (:supplied-p
           `(let ((the-value supplied-p-true))
              (setq supplied-p-true nil)
              the-value))
         (:aux
           `(let* ((the-value 
                     (if arglist
                        (first arglist)
                       ,(system::%get-plist (rest parsed-lambda-list-elt) :init-form))))
              the-value))
         (t (error "Internal error: make-args-for-apply-aux usage ..." (get-usage parsed-lambda-list-elt)))))
   parsed-lambda-list))

(defun make-args-for-apply (parsed-lambda-list)
 (unless (all-params-required-p parsed-lambda-list)
   `(lambda (arglist)
      (let ((supplied-p-true nil))
        (list ,@(make-args-for-apply-aux parsed-lambda-list))))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Conditional code to fill in init forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Here we handle the non-constant init forms specified in the parameter list. All of these
already wrapped in a lambda expression (no parameters) found in the parsed parameter list.
The interesting trick is to make sure the proper environment is maintained for the init
forms. For example, a parameter list such as
  (a &optional (b (+ a external)))
The definition of 'a' in the init form refers to the first parameter. However, the
definition of 'external' is a function of the surrounding environment. 'external' may be
a bound variable or it may be a special variable. That must be determined in the lexical
environment around the parameter list. It gets more interesting if the parameter list were
  (a &optional (b (+ a external)) (external (1+ b)))
Here the first reference to 'external' is to the surrounding environment (including 'a').
The second reference to 'external' as the third parameter. This reference is bound in the
environment that ends at 'b'. So the 2 'external's are different bound variables. Just to
make it more confused, the init form for 'external' involves the bound value of the
parameter 'b'.

Here is the parsed parameter list for the second example.
((a :scope :lexical :usage :required...)
 (b :scope :lexical :usage :optional        :init-form (lambda () (+ a external)))
 (external :scope :lexical :usage :optional :init-form (lambda () (1+ b))))
At this point in parameter handling, we are dependent on the compiler to understand the
proper environments. It has to treat this list as it would a LET* construct. That when the
compiler encounters an entry, it must compile the init form in the existing environment
prior it adding the new parameter to the environment. Furthermore, it must hang onto the
compiled lambda form for later use. Usually in the parsed parameter list.

In CLforJava, there are no optional arguments at the Java level (a white lie, but it will
suffice for now). Effectively, the arglist is treated as if the parameters were all
required. When an arglist is handed to the compiler, it processes it according to
the rules in the parameter list. This is accomplished by a function generated by the
parameter list parser. After validation, the arglist is extended if needed by filling in
Java null values for those parameters that the arglist did not suffice. During compilation,
the function was augmented with a series of tests. Each of these tests checks to see if it
has a non-null value (an actual argument was provided). If not, the test will funcall the
appropriate lambda expression previously compiled (or if the init form was a constant, the
constant is placed into the proper argument position).

Example from the second parameter list. If arglist is (42), then the following actions
follow.
1. The arglist is augmented to (42 null null).
2. The code that augmented the primary lambda expression, looks like:
  (if (eq b null) (funcall #'function-for-b))
  (if (eq external null) (funcall #'function-for-external)))
Since the init forms were compiled earlier, it looks almost as
  function-for-b -> (lambda () (+ a external))
  function-for-external -> (lambda () (1+ b))
This however would be incorrect to compile it in the environment, since the definition of
'external' in the 'b' init form would likely be incorrect or missing.

It is important in CLforJava to remember that functions are instances of Java classes that
implement the Function interface. It implies that, when the flow of computation encounters
the primary lambda form, it not only creates a new instance of the function, but it must
also create instances of the init form closures. This also implies a storage scheme to hold
those init form closures at the instance level.
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; End of Conditional code to fill in init forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
In this section, we take the association list created by parsing the lambda list and turn
it into a function that can be applied to an arg list. In effect, this is a code generator
for the parser that is the front-end of a lambda list compiler.
|#

;;; Takes a parsed lambda list produced by parse-ordinary-lambda-list
;;; and returns a list of the parameter names if all entries are
;;; required and NIL otherwise.

(defun all-params-required-p (parsed-lambda-list)
  (declare (system::%java-class-name "lisp.system.compiler.AllParamsRequiredP"))
  (catch 'found-non-required-param
    (system::%mapcar
      #'(lambda (param)
          (if (eq (get-usage param) :required)
              (get-name param)
            (throw 'found-non-required-param nil)))
      parsed-lambda-list)))

;;; Takes a parsed lambda produced by parse-ordinary-lambda-list
;;; and returns a function that takes an unevaluated argument list.
;;; The result of the function is the Lisp code necessary to
;;; correctly evaluate the arguments and apply the original function
;;; to the evaluated arguments.
;;;
;;; Note that this function creates the complete expression needed by the
;;; code generator to call the function.
;;;
;;; Examples:
;;; 1. All parameters are required. The arglist is checked for the
;;;    correct number of args and then just returns the arglist. Each
;;;    element will be evaluated in order and pushed on the Java stack.
;;;    For this example, assume that the arglist is (a b c).
;;; => #'(LAMBDA (#:G473 #:FUNCTION-MARKER)
;;;      (LET* ((#:G474 3)
;;;             (#:LENGTH-475 (LENGTH #:G473)))
;;;        (UNLESS (eql #:G474 #:LENGTH-475)
;;;          (ERROR "Incorrect number of required arguments to function, ~S" #:G474))
;;;        `(compiler::%function-marker% ,#:FUNCTION-MARKER ,@#:G473)))
;;;
;;; 2. One required param (A) and one optional (B). The default form is 42.
;;; => #'(LAMBDA (#:G473 #:FUNCTION-MARKER)
;;;      (LET* ((#:G474 1)
;;;             (#:LENGTH-475 (LENGTH #:G473)))
;;;        (UNLESS (<= #:G474 #:LENGTH-475)
;;;          (ERROR "Incorrect number of required arguments to function, ~S" #:G474))
;;;        `(compiler::%function-marker% ,#:FUNCTION-MARKER
;;;          ,(NTH #:G473 0) ; The required argument
;;;          ,(IF (> #:LENGTH-475 1) (NTH #:G473 1) 42)
;;;          ;; if the default form was (1+ A), then the last line will be
;;;          ,(IF (> #:LENGTH-475 1) (NTH #:G473 1) 'system::%java-null))))
;;;
;;;    If the function is applied to an arglist (1 X), the result is (1 X)
;;;    If the function is applied to an arglist (1), the result is   (1 42)
;;;    If the default form however is (1+ A), the result is          (1 system::%java-null)
;;;      In this case, code is inserted into the body of the function that tests
;;;      for the (Java) null value and funcalls the generated code - (lambda () (1+ A))
;;;      and ultimately the value for the 2nd argument would be 2.
;;;
;;; 3. One required param (A) and a REST argument (R)
;;; => #'(LAMBDA (#:G473 #:FUNCTION-MARKER)
;;;      (LET* ((#:G474 1)
;;;             (#:LENGTH-475 (LENGTH #:G473)))
;;;        (UNLESS (<= #:G474 #:LENGTH-475)
;;;          (ERROR "Incorrect number of required arguments to function, ~S" #:G474))
;;;        `(compiler::%function-marker% ,#:FUNCTION-MARKER
;;;          ,(NTH #:G473 0)       ; The required argument
;;;          (LIST ,@(NTHCDR #:G473 1))))) ; The REST argument
;;;
;;; 4. One required param (A), a REST argument (R), and a KEYWORD argument (key :K)
;;; => #'(LAMBDA (#:G473 #:FUNCTION-MARKER)
;;;      (LET* ((#:G474 1)
;;;             (#:LENGTH-475 (LENGTH #:G473)))
;;;        (UNLESS (<= #:G474 #:LENGTH-475)
;;;          (ERROR "Incorrect number of required arguments to function, ~S" #:G474))
;;;        `(let ((#:LET492 (LIST ,@(NTHCDR #:G473 1))))
;;;          (compiler::%function-marker% ,#:FUNCTION-MARKER
;;;            ,(NTH #:G473 0)       ; The required argument
;;;            #:LET492              ; The REST argument
;;;            (SYSTEM::%GET-PLIST #:LET492 ':K system::%java-null)))))
;;;
;;; 5. One required param (A), NO argument (R), and a KEYWORD argument (key :K)
;;; => #'(LAMBDA (#:Arglist-473 #:FUNCTION-MARKER)
;;;      (LET* ((#:ReqArgCount-474 1)
;;;             (#:ArglistLength-475 (LENGTH #:Arglist-473)))
;;;        (UNLESS (<= #:ReqArgCount-474 #:ArglistLength-475)
;;;          (ERROR "Incorrect number of required arguments to function, ~S" #:G474))
;;;        `(let ((#:LET492 (LIST ,@(NTHCDR #:Arglist-473 1))))
;;;          (compiler::%function-marker% ,#:FUNCTION-MARKER
;;;            ,(NTH #:Arglist-473 0)       ; The required argument
;;;            (SYSTEM::%GET-PLIST #:LET492 ':K system::%java-null)))))
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*rest-args-param* t)
(setq *rest-args-param* nil)
)

;;; FIRST-NTH - returns a copy of the first n conses of a list
(defun first-nth (list n)
  (when list
    (if (zerop n)
      nil
      (cons (car list) (first-nth (cdr list) (1- n))))))

;;; PAIRUP - a simple version of pairlis - which we don't have at this point
(defun pairup-aux (list-1 list-2 count)
  (when (and list-1 (plusp count))
    (cons (list (first list-1) (first list-2))
      (compiler::pairup-aux (rest list-1) (rest list-2) (1- count)))))

(defun pairup (list-1 list-2 count)
  (pairup-aux list-1 list-2 count))

;;; A simple function to check if the &rest variable is a fake
(defun rest-variable-fake-p (var)
  (eq var (fake-rest-form)))

;;; A simple form to change a %get-list arg
(defun change-variable-name (result-list rest-name)
  (when result-list
    (let ((elt (first result-list)))
      (when (and (listp elt) (eq (first elt) 'system::%get-plist))
        (rplaca (rest elt) rest-name))
      (cons elt (change-variable-name (rest result-list) rest-name)))))

(eval-when (:load-toplevel :execute)

;;; Takes a parsed lambda list and a generated symbol that will become the
;;; required parameter to the generated function. Returns the appropriate binding
;;; (let*) list to match parameters to arguments.
(defun kill-fake-entry (parsed-lambda-list)
  (when parsed-lambda-list
    (if (eq (caar parsed-lambda-list) (fake-rest-form))
      (kill-fake-entry (rest parsed-lambda-list))
      (cons (first parsed-lambda-list) (kill-fake-entry (rest parsed-lambda-list))))))

(defun build-lambda-elements (parsed-lambda-list arglist-param-sym arglist-length-sym fn)
  (declare (system::%java-class-name "lisp.system.compiler.BuildLambdaLists"))
  ;; ** ok, I have to remove any instances of the Fake entry. ** mumble ** grumble **
  (setq parsed-lambda-list (kill-fake-entry parsed-lambda-list))

  ;; ---------------- Section 1 ---------------------
  (let ((result nil)
        (count 0)
        (optional-p-count (cdr (count-parameters parsed-lambda-list :optional)))
        (key-p-count      (cdr (count-parameters parsed-lambda-list :key)))
        (let-variable (gensym "LetVariable-"))
        (rest-or-key (rest-or-key-param-p parsed-lambda-list))) ;; (:key.. or (:rest...    ))
    ;; sets up the forms for &rest and &key
    (setq *rest-args-param*
      (let ((param-type rest-or-key))
        ;; param-type => (:rest_or_:key the_parsed-lambda-list_elt) - if type is :key, have to fake it
        (if param-type
          (list (get-name (second param-type)) (count-req-and-opt-params parsed-lambda-list (first param-type)))
          nil)))

    ;; better when we get the do macro...
    (tagbody
      top
      (when parsed-lambda-list
        ;; remove the nil elements
        (setq result
              `(,@(system::%mapcan
                    #'(lambda (x) (and x (list x)))
                    (build-lambda-element
                      (first parsed-lambda-list) count arglist-param-sym arglist-length-sym))
                ,@result))
        (setq parsed-lambda-list (rest parsed-lambda-list))
        (unless (eq (get-usage (first parsed-lambda-list)) :supplied-p)
          (setq count (1+ count)))
        (go top)))

    ;; unreverse the list
    (setq result (system::%reverse result))

    ;; ---------------- Section 2 ---------------------

    ;; Now count carries the count of all of the elts (includes the supplied-p's)
    ;; The second part of *rest-args-param* carries the number of req and opt params
    ;; when it gets out of the loop, if there is a value for *rest-args-param*
    ;; it creates a LET* binding of the param to a reference to the &rest of
    ;; the arglist. If there is no &rest or &key params, it does not create the
    ;; LET form.
    (if *rest-args-param*
        (let ((&rest-name (first *rest-args-param*))
              (req-and-opt-count (second *rest-args-param*)))
          (let* ((altered-req-opt-p-count (- req-and-opt-count optional-p-count))
                 (fake-needed (eq (first rest-or-key) :key))
                 (result-length (if fake-needed (length result) (1+ (length result))))
                 (gensym-arg-list (make-gensym-list (if fake-needed result-length (1- result-length)))))
            (unless fake-needed   ; puts in the real &rest param name if needed
              (setq &rest-name (nth (- req-and-opt-count optional-p-count) gensym-arg-list))
              ;; now run down the current result, changing the (system::%get-plist ...) to &rest-name
              (setq result (change-variable-name result &rest-name)))
            (let* ((paired-gensym-req-opt (pairup gensym-arg-list result req-and-opt-count)) ;;result
                   (first-part-of-parser
                     (system::%mapcar
                       #'(lambda (x) `(identity (list (list ',(first x) ,@(rest x)))))
                       paired-gensym-req-opt))
                   (second-part-of-parser
                     (if fake-needed
                       (intern (symbol-name (gensym "FakeRest-")))
                       (nth (length paired-gensym-req-opt) gensym-arg-list)))
                   (fourth-part-of-parser
                     (system::%mapcar
                       #'(lambda (x)
                          (let ((sys-%get-form (cadadr x)))
                            (when (eq (first sys-%get-form) 'system::%get-plist)
                              (rplaca (rest sys-%get-form) second-part-of-parser)))
                          `(identity (list (list ',(first x) ,@(rest x)))))
                       (pairup (nthcdr (if fake-needed req-and-opt-count (1+ req-and-opt-count)) gensym-arg-list)
                               (nthcdr (if fake-needed req-and-opt-count (1+ req-and-opt-count)) (nthcdr 0 result))
                               (- result-length req-and-opt-count)))))
                   ;;-> This is where I need to add a fifth parser part to deal with &allow-other-keys.
                   ;;-> This has to put in code to check for other keys and if Ok, then check they are paired.
                   ;;-> The last is to ??hmm, when/where do I put code for handling :allow-other-keys
              (let ((output
                      `(list 'let*
                         `( ,@,@first-part-of-parser
                            (,',second-part-of-parser (list ,@(nthcdr ,altered-req-opt-p-count ,arglist-param-sym)))
                            ,@,@fourth-part-of-parser)
                         `(compiler::%function-marker% ,,fn ,@',gensym-arg-list))))
                (setq *rest-args-param* nil)     ;* clear the special variable
                output))))
        `(list 'compiler::%function-marker% ,fn ,@result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pairup-top (pairs args)
  (when pairs
    (cons
      (if (eq (first (second (first pairs)))  '>)
        (car pairs)
        (if args
          (list (caar pairs) (first args))
          (car pairs)))
      (pairup-top (rest pairs) (rest args)))))

;;; This function provide count of components in a lambda list. They return
;;; a cons (MV isn't done at this time). The first value is the number of
;;; basic parameters EXCLUDING any supplied-p in these set. The second value
;;; is the number of supplied-p parameters in that section of the lambda list.

(defun count-parameters (parsed-lambda-list key)
  (let ((default (cons 0 0))
        (count 0)
        (supplied 0))
    (tagbody
      top
      (let ((parsed (first parsed-lambda-list)))
        (when parsed
          (if (eq (get-usage parsed) key)
            (progn
              (setq count (1+ count))
              (setq parsed-lambda-list (rest parsed-lambda-list))
              ;; if has-supplied-p optional tag, check for it
              (when (eq (get-usage (first parsed-lambda-list)) :supplied-p)
                (setq supplied (1+ supplied))
                (setq parsed-lambda-list (rest parsed-lambda-list))))
            (setq parsed-lambda-list (rest parsed-lambda-list)))
          (go top))))
    (cons count supplied)))



;;; Counts the number of required and optional parameters in a lambda list. This is used
;;; to determine how offset in the arglist that starts the &rest arguments. If there
;;; is no explicit &rest parameter, it counts the params up to the first &key param.
(defun count-req-and-opt-params (parsed-lambda-list param-type)
  (declare (system::%java-class-name "lisp.system.compiler.CountReqAndOptParams"))
  (let ((count 0))
    (tagbody
      top
      (when (and parsed-lambda-list (not (eq param-type (get-usage (first parsed-lambda-list)))))
        (setq count (1+ count))
        (setq parsed-lambda-list (rest parsed-lambda-list))
        (go top)))
    count))

;;; provides a list of the key names in the parameter list.
;;; This is useful for editing and runtime checking.
(defun make-key-list (parsed-lambda-list)
  (system::%mapcan
    #'(lambda (parsed-lambda-elt)
       (when (eq (get-usage parsed-lambda-elt) :key)
         (list (get-key-name parsed-lambda-elt))))
    parsed-lambda-list))

;;; This provides a list of gensym'd symbols used in setting up the actual LET* form
;;; for evaluating the arguments in the proper sequence.
(defun make-gensym-list (count)
  (when (plusp count)
    (cons (intern (symbol-name (gensym))) (make-gensym-list (1- count)))))

;;; Given a parsed lambda parameter, it creates the code to generate the code to handle the argument
;;; that matches that parameter.
(defun build-lambda-element (parsed-lambda-elt count arglist-param-sym arglist-length-sym)
  (declare (system::%java-class-name "lisp.system.compiler.BuildLambdaElement"))
  (let ((usage (get-usage parsed-lambda-elt)))
    (cond ((eq usage :required)
           (create-required-binding parsed-lambda-elt count arglist-param-sym))
          ((eq usage :optional)
           (create-optional-binding parsed-lambda-elt count arglist-param-sym arglist-length-sym))
          ((eq usage :rest)
           (create-rest-binding parsed-lambda-elt count))
          ((eq usage :supplied-p)
           (create-supplied-p-binding parsed-lambda-elt count arglist-param-sym))
          ((eq usage :key)
           (create-key-binding parsed-lambda-elt count arglist-param-sym arglist-length-sym))
          ((eq usage :allow-other-keys)
           ;; make code that will check about the key args
           nil))))

;;; create-required-binding
(defun create-required-binding (parsed-lambda-elt count arglist-param-sym)
  (declare (system::%java-class-name "lisp.system.compiler.CreateRequiredBinding"))
  (setq *lambda-elements-required* (1+ *lambda-elements-required*))
  `(`,(nth ,count ,arglist-param-sym)))

;;; This function has 2 modes:
;;; 1. When an optional param default form is a constant, it lays down that constant
;;;    in the actual arglist (it's self-evaluating).
;;; 2. When an optional param default form is not a constant. The form must be evaluated
;;;    within the body of the function. The actual argument is a Java null.
;;;          ,(IF (> #:LENGTH-475 1) (NTH #:G473 1) 42)
;;;          ;; if the default form was (1+ A), then the last line will be
;;;          ,(IF (> #:LENGTH-475 1) (NTH #:G473 1) 'system::%java-null)

(defun create-optional-binding (parsed-lambda-elt count arglist-param-sym arglist-length-sym)
  (declare (system::%java-class-name "lisp.system.compiler.CreateOptionalBinding"))
  `(`,(if (> (length ,arglist-param-sym) ,count)
        (nth ,count ,arglist-param-sym)
        'system::%java-null)))

(defun create-supplied-p-binding (parsed-lambda-elt count arglist-param-sym arglist-length-sym)
  (declare (system::%java-class-name "lisp.system.compiler.CreateSuppliedPBinding"))
  (setq *lambda-elements-required* (1+ *lambda-elements-required*))
  `((> (length ,arglist-param-sym) ,count)))

(defun create-rest-binding (parsed-lambda-elt count)
  (declare (system::%java-class-name "lisp.system.compiler.CreateRestBinding"))
  ;; hang onto the bound variable
  (when (get-allocation parsed-lambda-elt) `(,(first *rest-args-param*))))

(defun create-key-binding (parsed-lambda-elt count arglist-param-sym arglist-length-sym)
  (declare (system::%java-class-name "lisp.system.compiler.CreateKeyBinding"))
  `((quote (system::%get-plist
             ,(first *rest-args-param*)
             ,(get-key-name parsed-lambda-elt)
             ,'system::%java-null))))

(defun required-param-count (parsed-lambda-list)
  (declare (system::%java-class-name "lisp.system.compiler.RequiredParamCount"))
  (let ((count 0)
        (lcl parsed-lambda-list))
    (tagbody
       top
       (if lcl
         (let ((usage (get-usage (first lcl))))
          (when (eq usage :required)
            (setq count (1+ count)))
          (setq lcl (rest lcl))
          (go top))))
    count))

(defun rest-or-key-param-p (parsed-lambda-list)
  (declare (system::%java-class-name "lisp.system.compiler.RestOrKeyParamP"))
  (when parsed-lambda-list
    (let ((parsed-lambda-list-elt (first parsed-lambda-list)))
      (if (eq (get-usage parsed-lambda-list-elt) :rest)
        `(:rest ,parsed-lambda-list-elt)
        (if (eq (get-usage parsed-lambda-list-elt) :key)
          `(:key ,parsed-lambda-list-elt)
          (rest-or-key-param-p (rest parsed-lambda-list)))))))

;;; Here's a function that can spit out a function that checks for the names of
;;; the keys (if any) defined in the parameter list. At runtime, the function will
;;; check all of the supplied keys are defined. If not, it throws an error. Unless
;;; the &allow-other-keys found in the lambda list or if the lambda list contains
;;; the key :allow-other-keys is present and true.
(defun check-correct-keys (parsed-lambda-list)
  (let ((keys (system::%mapcan
                #'(lambda (elt)
                    (if (eq (get-usage elt) :key) (get-name elt) nil))
                parsed-lambda-list)))
    (when keys ;; this can be overridden by :allow-other-keys or &allow-other-keys
      #'(lambda (key) (cl::quick-member key keys)))))

;;; These are the handy accessors for elements in a parsed list

(defun get-usage (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetUsage"))
  (system::%get-plist (rest parsed-lambda-list-element) :usage))

(defun get-name (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetName"))
  (first parsed-lambda-list-element))

(defun get-init-form (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetInitForm"))
  (system::%get-plist (rest parsed-lambda-list-element) :init-form))

(defun get-allocation (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetAllocation"))
  (system::%get-plist (rest parsed-lambda-list-element) :allocation))

(defun get-key-name (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetKeyName"))
  (system::%get-plist (rest parsed-lambda-list-element) :key-name))

(defun get-supplied-p (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetSuppliedP"))
  (system::%get-plist (rest parsed-lambda-list-element) :supplied-p))

(defun get-has-supplied-p (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetHasSuppliedP"))
  (system::%get-plist (rest parsed-lambda-list-element) :has-supplied-p))

(defun get-allow-other-keys-p (parsed-lambda-list-element)
  (declare (system::%java-class-name "lisp.system.compiler.GetAllowOtherKeys"))
  (system::%get-plist (rest parsed-lambda-list-element) '&allow-other-keys))

;;; A template for inserting arglist tests into the beginning of a primary lambda
;;; expression.

(defun provide-init-form-usage (parsed-lambda-list)
  (declare (system::%java-class-name "lisp.system.compiler.ProvideInitFormUsage"))
  (system::%mapcan
    #'(lambda (parsed-lambda-list-elt)
        (let ((usage (get-usage parsed-lambda-list-elt)))
          (if (or (eq usage :optional) (eq usage :key))
            (list
              (let ((init-form (get-init-form parsed-lambda-list-elt)))

                ;;*** HACK ALERT ***
                ;;*** The old compiler can't handle a naked symbol as a
                ;;*** bound variable and the init form in an &optional
                (when (symbolp init-form) (setq init-form `(identity ,init-form)))

                ;; Generates code to evaluate the init-form if the bound symbol
                ;; has no value (system::%java-null). Then if it has no value, there
                ;; was no value given - hence a possible supplied-p variable gets NIL.
                ;; But if the bound symbol has a value, it was given a value (even if NIL).
                ;; Therefore, the supplied-p variable will get T
                (let ((the-supplied-p-symbol (system::%get-plist (rest parsed-lambda-list-elt) :has-supplied-p)))
                  `(if (eq ,(first parsed-lambda-list-elt) system::%java-null)
                    (progn
                      ,@(when the-supplied-p-symbol
                        `((setq ,the-supplied-p-symbol nil)))
                      ,(if (or (simple-constantp init-form) (symbolp init-form))
                        `(setq ,(first parsed-lambda-list-elt) ,init-form)
                        `(setq ,(first parsed-lambda-list-elt) (funcall (function (lambda () ,init-form))))))
                    ,@(when the-supplied-p-symbol
                      `((setq ,the-supplied-p-symbol t))))))))))
    parsed-lambda-list))

(defun generate-arglist-analyzer (parsed-lambda-list)
  (declare (system::%java-class-name "lisp.system.compiler.GenerateArglistAnalyzer"))
  (let ((x 1)
        (arglist-param-sym (intern (symbol-name (gensym "Arglist-"))))
        (required-arg-count (intern (symbol-name (gensym "ReqArgCount-"))))
        (arglist-length-sym (intern (symbol-name (gensym "ArglistLength-"))))
        (rest-let (intern (symbol-name (gensym "RestLet-"))))
        (function-marker (intern (symbol-name (gensym "FunctionMarker-")))))
    `(lambda (,arglist-param-sym ,function-marker)
        (declare (compiler::%no-generate-analyzer))
        (let ((,required-arg-count ,(required-param-count parsed-lambda-list))
              (,arglist-length-sym (length ,arglist-param-sym)))
          (unless (<= ,required-arg-count ,arglist-length-sym)
            (error "Too few required arguments for the function, ~S" ,function-marker
                   ',(system::%mapcar #'car parsed-lambda-list) ,arglist-length-sym ,(required-param-count parsed-lambda-list)))
          ;; nothing else to say if all required
          ,(if (all-params-required-p parsed-lambda-list)
            `(if (eql ,required-arg-count ,arglist-length-sym)
               (append (list 'compiler::%function-marker% ,function-marker) ,arglist-param-sym) ; it's the actual arg/param list to the function
               (error "Wrong number of arguments (~S) to function, ~S"
                 ", function marker: " ,function-marker
                 ", required param count: " ,required-arg-count
                 ", actual list count: " ,arglist-length-sym
                 ", arglist: " ,arglist-param-sym
                 ", parsed-lambda-list: " ',parsed-lambda-list))
            (build-lambda-elements
              parsed-lambda-list arglist-param-sym arglist-length-sym function-marker))))))

) ;eval-when
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;; Now comes the next piece of magic... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; ************* DEFMACRO **************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-parse-whole (parsed-lambda-list)
  (caar parsed-lambda-list))

(defun get-parse-env (parsed-lambda-list)
  (or (system::%mapcan #'(lambda (elt)
              (when (eq (get-usage elt) :environment)
                (return-from get-parse-env (get-name elt))))
         parsed-lambda-list)
    (gensym "%ENV-")))

(eval-when (:compile-toplevel :load-toplevel :execute)
(system::%set-special '*parameter-offset* t)(setq *parameter-offset* 1)
(system::%set-special '*top-level* t)(setq *top-level* t)
(system::%set-special '*current-whole-name* t)(setq *current-whole-name* nil)
)

(defun letify-parsed-macro-lambda-list (parsed-lambda-list)
  (let ((*current-whole-name* *current-whole-name*)
        (*parameter-offset* *parameter-offset*))
    (system::%mapcan #'letify-parsed-macro-lambda-elt parsed-lambda-list)))

(defun letify-parsed-macro-lambda-elt (parsed-lambda-elt)
  (unless (and (not (listp (first parsed-lambda-elt)))
            (eq (get-usage parsed-lambda-elt) :environment))
    (handle-parsed-macro-lambda-elt parsed-lambda-elt)))

(defun handle-parsed-macro-lambda-elt (parsed-lambda-elt)
  (if (listp (first parsed-lambda-elt)) ; a destructuring form
    (let ((*top-level* nil))
      (let ((let-list (letify-parsed-macro-lambda-list parsed-lambda-elt)))
        (setq *parameter-offset* (1+ *parameter-offset*))
        let-list))
    (case (get-usage parsed-lambda-elt)
      (:whole
        ;; two possibilities
        ;; 1. This is the top level &whole. This name is bound in the param-list
        ;;    to the macro. Hence there is no let* binding for the name.
        (if *top-level*
          (progn
            (setq *current-whole-name* (get-name parsed-lambda-elt))
            nil)
          (let ((current-whole-name *current-whole-name*)
                (parameter-offset *parameter-offset*))
            (setq *current-whole-name* (get-name parsed-lambda-elt))
            (setq *parameter-offset* 0)
            `((,*current-whole-name* (nth ,parameter-offset ,current-whole-name))))))
      (:required
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(get-name parsed-lambda-elt) (nth ,parameter-offset ,*current-whole-name*)))))
      (:optional ;;//TODO - add support for supplied-p
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(get-name parsed-lambda-elt)
             (if (nthcdr ,parameter-offset ,*current-whole-name*)
               (nth ,parameter-offset ,*current-whole-name*)
               ,(get-init-form parsed-lambda-elt))))))
      ((:rest :body)
        (let ((parameter-offset *parameter-offset*)
              (current-whole-name *current-whole-name*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          (setq *current-whole-name* (get-name parsed-lambda-elt))
          `((,*current-whole-name* (nthcdr ,parameter-offset ,current-whole-name)))))
      (:key  ;;//TODO - add support for supplied-p
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(get-name parsed-lambda-elt)
             (if (cl::quick-member ',(get-key-name parsed-lambda-elt) ,*current-whole-name*)
               (system::%get-plist ,*current-whole-name* ,(get-key-name parsed-lambda-elt))
               ,(get-init-form parsed-lambda-elt))))))
      (:aux `((,(get-name parsed-lambda-elt) ,(get-init-form parsed-lambda-elt))))
      (t (error "Illegal parameter usage type ~S with parameter ~S"
           (get-usage parsed-lambda-elt)
           (get-name parsed-lambda-elt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The interesting aspect of this piece of code is that it replaces itself...

(defun declarationp (form)
  (and (listp form) (eq (first form) 'declare)))

;;; This function breaks the declarations and strings from the executable component
;;; and returns the true body.
(defun decls-and-strings (body)
  (cond ((null body) nil)
        ((and (endp (rest body)) (stringp (first body))) body)
        ((stringp (first body)) (decls-and-no-strings (rest body)))
        ((declarationp (first body)) (decls-and-strings (rest body)))
        (t body)))

(defun decls-and-no-strings (body)
  (cond ((null body) nil)
        ((declarationp (first body)) (decls-and-no-strings (rest body)))
        (t body)))

(defun snip-full-body (full-body body)
  (unless (eq full-body body)
    (cons (first full-body) (snip-full-body (rest full-body) body))))

(defmacro defmacro1 (whole env)
  (declare (system::%java-class-name "lisp.common.function.Defmacro"))
  ;; first is to break down the components
  ;; ignore the name and the macro name...
  ;; There can be many types of lambda list. This is used to figure which one:
  (let* ((parsed-lambda-list (parse-macro-lambda-list (third whole)))
         (name (second whole))
         (full-body (nthcdr 3 whole))
         (body (decls-and-strings full-body))
         ;; This line divides the full-body (with decls) into 2 sections:
         ;; decls section and the body proper
         (decls-etc (snip-full-body full-body body)))
    `(progn
       (common-lisp::set-symbol-function
         (quote ,(second whole))
         (common-lisp::macro-lambda (,(get-parse-whole parsed-lambda-list) ,(get-parse-env parsed-lambda-list))
             (declare (system::%lisp-name ,name)) ,@decls-etc
             (block ,name
               (let* ,(letify-parsed-macro-lambda-list parsed-lambda-list)
                 ,@body))))
       (system::set-function-field (quote ,name))
       (quote ,name))))


;;;; HACK ALERT ;;;;
;;; These definitions are temporary implementations until we get the global environment more stable...
(eval-when (:compile-toplevel :load-toplevel :execute)
(common-lisp:export
  '(common-lisp::make-package common-lisp::gentemp)
  (find-package "COMMON-LISP"))
); eval-when
;;;;; END HACK ALERT ;;;;

;;;;;

;;; Now for some of the magic of re-implementing the major macros - DEFMACRO and DEFUN
(eval-when (:load-toplevel :execute)
(defmacro1 defun1 (fn-name parameter-list &body full-body)
  (declare (system::%java-class-name "lisp.common.function.Defun"))
    (if (symbolp fn-name)
      (let* ((parsed-lambda-list (parse-ordinary-lambda-list parameter-list))
             (body (decls-and-strings full-body))
             (decls-etc (snip-full-body full-body body)))
        (let ((arglist-munge (compiler::make-args-for-apply parsed-lambda-list)))
          `(progn
             (common-lisp::set-symbol-function
               (quote ,fn-name)
               (common-lisp:lambda (,@parameter-list)
                 (declare (system::%lisp-name ,fn-name)) ,@decls-etc
                 (block ,fn-name ,@body)))
             (system::set-function-field (quote ,fn-name))
             ;; take the parameter list, use it to create a fn that checks the args
             ;; it's usually for APPLY
             (system::%set-function-arglist-munger (symbol-function ',fn-name) ,arglist-munge)
             (quote ,fn-name))))
      (if (and (listp fn-name) (eq (car fn-name) 'common-lisp::setf))
        (let* ((parsed-lambda-list (parse-defsetf-lambda-list parameter-list))
               (body (decls-and-strings full-body))
               (decls-etc (snip-full-body full-body body))
               (parse-fn-name (cadr fn-name)))
          ;; now we have to put this lambda into the function's setf slot
          `(progn
             (system::%set-setf-function
               (quote ,parse-fn-name)
               (common-lisp:lambda (,@parameter-list)
                 (declare (system::%lisp-name ,(gensym parse-fn-name))) ,@decls-etc
                 (block ,parse-fn-name ,@body)))
             (quote ,fn-name)))
        (error "Improper function name, ~S, to DEFUN: " fn-name))))

(defmacro1 lambda1 (args &rest body)
  `(function (lambda ,args ,@body)))

;;; Now the magic. We slip the new defmacro and defun macro functions
;;; into the symbols defmacro and defun. And we slip in the function code (instance)
;;; into the CommonLispFunctions fields. Eventually the old code will be garbage collected.
(common-lisp::set-symbol-function 'common-lisp::defmacro (symbol-function 'defmacro1))
(system::set-function-field 'common-lisp::defmacro)

(common-lisp::set-symbol-function 'common-lisp::defun (symbol-function 'defun1))
(system::set-function-field 'common-lisp::defun)

(common-lisp::set-symbol-function 'common-lisp::lambda (symbol-function 'lambda1))
(system::set-function-field 'common-lisp::lambda)
#|
|#
); eval-when

(eval-when (:load-toplevel :execute)
(provide "LambdaListParsersLsp")
(require "PrinterLsp")
) ;eval-when
