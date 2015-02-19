package jcl.compiler.real.sa.analyzer.expander;

import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

/**
 * DefMacroExpander takes two arguments.  The first is the list containing the Defmacro symbol,
 * the name of the function, the list of its parameters, and the body of the function.
 * The second argument is the environment.
 * <p>
 * If DefMacroExpander receives the list (defmacro foo (x) (+ x 7)),
 * it will return (PROGN (SETSYMBOLFUNCTION (QUOTE FOO) (MACRO-LAMBDA (X) (+ X 7))) (QUOTE FOO))
 * <p>
 * The call to DefMacroExpander would look like this :
 * (%DEFMACRO-EXPANDER (LIST 'DEFMACRO 'FOO '(X) '(+ X 7)) '(ENV))
 */
public class DefMacroExpander extends DefunExpander {

	public static final DefMacroExpander FUNCTION = new DefMacroExpander();

	@Override
	protected ListStruct setTypeSymbol(ListStruct mostlyLambda) {
		return new ConsStruct(SpecialOperator.LAMBDA, mostlyLambda);
	}

	/*

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
    (system::%mapcan #'letify-parsed-macro-lambda-elt
      parsed-lambda-list)))

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
      (:optional ;;//TODOO - add support for supplied-p
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
      (:key ;;//TODOO - add support for supplied-p
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(get-name parsed-lambda-elt)
             (if (cl::quick-member ',(get-key-name parsed-lambda-elt) ,*current-whole-name*)
                 (system::%get-plist ,*current-whole-name* ,(get-key-name parsed-lambda-elt))
               ,(get-init-form parsed-lambda-elt))))))
      (:aux
        `((,(get-name parsed-lambda-elt) ,(get-init-form parsed-lambda-elt))))
      (t
       (error "Illegal parameter usage type ~S with parameter ~S"
              (get-usage parsed-lambda-elt)
              (get-name parsed-lambda-elt))))))

(defun declarationp (form)
  (and (listp form) (eq (first form) 'declare)))

;;; This function breaks the declarations and strings from the executable component and returns the true body.
(defun decls-and-strings (body)
  (cond ((null body)
         nil)
        ((and (endp (rest body)) (stringp (first body)))
         body)
        ((stringp (first body))
         (decls-and-no-strings (rest body)))
        ((declarationp (first body))
         (decls-and-strings (rest body)))
        (t
         body)))

(defun decls-and-no-strings (body)
  (cond ((null body)
         nil)
        ((declarationp (first body))
         (decls-and-no-strings (rest body)))
        (t
         body)))

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

	 */
}
