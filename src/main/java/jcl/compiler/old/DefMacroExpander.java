package jcl.compiler.old;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class DefMacroExpander extends DefunExpander {

	// TODO:
	// 1. Add MACRO-LAMBDA SpecialOperator
	// 2. Add MACRO-LAMBDA SpecialOperator Analyzer
	// 3. Have Analyzer parse Lambda Lists as Macro Lambda Lists

	@Override
	public LispStruct expand(final ListStruct form) {

		final ListStruct allButDefun = form.getRest();
		final LispStruct macroName = allButDefun.getFirst();
		final ListStruct lambdaParamsAndBody = allButDefun.getRest();

		// Create and add: (progn (set-symbol[-setf]-function (quote foo) (lambda (x) (block foo x))) (quote foo))
		final List<LispStruct> expandedForm = new ArrayList<>();
		expandedForm.add(SpecialOperator.PROGN);

		final List<LispStruct> setFunction = new ArrayList<>();

		if (!(macroName instanceof SymbolStruct)) {
			throw new RuntimeException("Improper macro name supplied to DEFMACRO: " + macroName);
		}

		// Create: (quote foo)
		final List<LispStruct> quoteFunctionName = new ArrayList<>();
		quoteFunctionName.add(SpecialOperator.QUOTE);
		quoteFunctionName.add(macroName);

		final ListStruct quoteFunctionNameList = ListStruct.buildProperList(quoteFunctionName);
		setFunction.add(quoteFunctionNameList);
		// Add: (quote foo)

		// Create: (lambda (x) (block foo x))
		final List<LispStruct> lambdaForm = new ArrayList<>();
		lambdaForm.add(SpecialOperator.LAMBDA);

		// Create: (x)
		final LispStruct lambdaParams = lambdaParamsAndBody.getFirst();
		lambdaForm.add(lambdaParams);
		// Add: (x)

		// Create: (block foo x)
		final List<LispStruct> blockForm = new ArrayList<>();
		blockForm.add(SpecialOperator.BLOCK);
		blockForm.add(macroName);

		final ListStruct lambdaBody = lambdaParamsAndBody.getRest();
		final List<LispStruct> lambdaBodyList = lambdaBody.getAsJavaList();
		blockForm.addAll(lambdaBodyList);

		final ListStruct blockFormList = ListStruct.buildProperList(blockForm);
		lambdaForm.add(blockFormList);
		// Add: (block foo x)

		final ListStruct lambdaFormList = ListStruct.buildProperList(lambdaForm);
		setFunction.add(lambdaFormList);
		// Add: (lambda (x) (block foo x))

		final ListStruct setFunctionList = ListStruct.buildProperList(setFunction);
		expandedForm.add(setFunctionList);
		// Add: (set-symbol[-setf]-function (quote foo) (lambda (x) (block foo x)))

		expandedForm.add(quoteFunctionNameList);
		// Add (to end): (quote foo)

		// Return: (progn (set-symbol[-setf]-function (quote foo) (lambda (x) (block foo x))) (quote foo))
		return ListStruct.buildProperList(expandedForm);
	}

	@Override
	protected SpecialOperator getLambdaSymbol() {
		return SpecialOperator.MACRO_MARKER;
	}

	/*

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
               (eq (system::%get-plist (rest parsed-lambda-elt) :usage) :environment))
    (handle-parsed-macro-lambda-elt parsed-lambda-elt)))

(defun handle-parsed-macro-lambda-elt (parsed-lambda-elt)
  (if (listp (first parsed-lambda-elt)) ; a destructuring form
      (let ((*top-level* nil))
        (let ((let-list (letify-parsed-macro-lambda-list parsed-lambda-elt)))
          (setq *parameter-offset* (1+ *parameter-offset*))
          let-list))
    (case (system::%get-plist (rest parsed-lambda-elt) :usage)
      (:whole
        ;; two possibilities
        ;; 1. This is the top level &whole. This name is bound in the param-list
        ;;    to the macro. Hence there is no let* binding for the name.
        (if *top-level*
            (progn
              (setq *current-whole-name* (first parsed-lambda-elt))
              nil)
          (let ((current-whole-name *current-whole-name*)
                (parameter-offset *parameter-offset*))
            (setq *current-whole-name* (first parsed-lambda-elt))
            (setq *parameter-offset* 0)
            `((,*current-whole-name* (nth ,parameter-offset ,current-whole-name))))))
      (:required
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(first parsed-lambda-elt) (nth ,parameter-offset ,*current-whole-name*)))))
      (:optional ;;//TODOO - add support for supplied-p
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(first parsed-lambda-elt)
             (if (nthcdr ,parameter-offset ,*current-whole-name*)
                 (nth ,parameter-offset ,*current-whole-name*)
               ,(system::%get-plist (rest parsed-lambda-elt) :init-form))))))
      ((:rest :body)
        (let ((parameter-offset *parameter-offset*)
              (current-whole-name *current-whole-name*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          (setq *current-whole-name* (first parsed-lambda-elt))
          `((,*current-whole-name* (nthcdr ,parameter-offset ,current-whole-name)))))
      (:key ;;//TODOO - add support for supplied-p
        (let ((parameter-offset *parameter-offset*))
          (setq *parameter-offset* (1+ *parameter-offset*))
          `((,(first parsed-lambda-elt)
             (if (cl::quick-member ',(system::%get-plist (rest parsed-lambda-elt) :key-name) ,*current-whole-name*)
                 (system::%get-plist ,*current-whole-name* ,(system::%get-plist (rest parsed-lambda-elt) :key-name))
               ,(system::%get-plist (rest parsed-lambda-elt) :init-form))))))
      (:aux
        `((,(first parsed-lambda-elt) ,(system::%get-plist (rest parsed-lambda-elt) :init-form))))
      (t
       (error "Illegal parameter usage type ~S with parameter ~S"
              (system::%get-plist (rest parsed-lambda-elt) :usage)
              (first parsed-lambda-elt))))))

(defun get-parse-env (parsed-lambda-list)
  (or (system::%mapcan #'(lambda (elt)
                           (when (eq (system::%get-plist (rest elt) :usage) :environment)
                             (return-from get-parse-env (first elt))))
        parsed-lambda-list)
      (gensym "%ENV-")))

(defmacro defmacro1 (whole env)
  (declare (system::%java-class-name "lisp.common.function.Defmacro"))
  ;; first is to break down the components
  ;; ignore the name and the macro name...
  ;; There can be many types of lambda list. This is used to figure which one:
  (let* ((parsed-lambda-list (parse-macro-lambda-list (third whole)))
         (name (second whole))
         (full-body (nthcdr 3 whole))
         (body (decls-and-strings full-body))
         (decls-etc (snip-full-body full-body body)))
    `(progn
       (common-lisp::set-symbol-function
         (quote ,(second whole))
         (common-lisp::macro-lambda (,(caar parsed-lambda-list) ,(get-parse-env parsed-lambda-list))
             ,@decls-etc
             (block ,name
               (let* ,(letify-parsed-macro-lambda-list parsed-lambda-list)
                 ,@body))))
       (quote ,name))))
	 */
}
