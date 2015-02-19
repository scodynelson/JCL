package jcl.compiler.real.sa.analyzer.expander;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.List;

/**
 * DefunExpander takes two arguments.  The first is the list containing the Defun symbol,
 * the name of the function, the list of its parameters, and the body of the function.
 * The second argument is the environment.
 * <p>
 * If DefunExpander receives the list (defun foo (x) (+ x 7)),
 * it will return (PROGN (SET-SYMBOL-FUNCTION (QUOTE FOO) (LAMBDA (X) (+ X 7))) (QUOTE FOO))
 * <p>
 * The call to DefunExpander would look like this : (%DEFUN-EXPANDER (LIST 'DEFUN 'FOO '(X) '(+ X 7)) '(ENV))
 */
public class DefunExpander implements MacroFunctionExpander {

	//build necessary symbols for defun : SET-SYMBOL-FUNCTION, QUOTE, LAMBDA.
	// place to hang onto declarations and a doc string
	private ListStruct declsAndDoc = NullStruct.INSTANCE;

	// expands the defun macro
	public LispStruct expand(Object form, Object env) {
		ListStruct listToExpand = (ListStruct) form;
		// (defun foo (args) (decls or doc...) theBody)
//        Function1 printFn = (Function1)lisp.common.function.Print.FUNCTION;

		//sets functionName to the name of the function passed in the list.
		ListStruct listCdr = listToExpand.getRest();
		// (foo (args) (decls or doc...) theBody))
		SymbolStruct<?> functionName = (SymbolStruct) listCdr.getFirst();

		// create the function name declaration
		ListStruct nameDeclareClause = ListStruct.buildProperList(Declaration.LISP_NAME, functionName);
		ListStruct nameDeclaration = ListStruct.buildProperList(SpecialOperator.DECLARE, nameDeclareClause);

		//set parameterList to the list of parameters passed in the list.
		listCdr = listCdr.getRest();
		// ((args) (decls or doc...) theBody))
		ListStruct parameterList = (ListStruct) listCdr.getFirst();

		// handle any decls or doc string
		ListStruct maybeDeclsOrString = listCdr.getRest();
		// ((decls or doc...) theBody))
		ListStruct functionBody = processDeclsOrString(maybeDeclsOrString);
		// (theBody)

		//build a list containing the lambda symbol, the function parameters, and the function body
		ListStruct result = new ConsStruct(functionName, functionBody);
		result = new ConsStruct(SpecialOperator.BLOCK, result);
		result = new ConsStruct(result, NullStruct.INSTANCE);
		result = new ConsStruct(nameDeclaration, result);
		if (declsAndDoc != NullStruct.INSTANCE) {
			final List<LispStruct> declsAndDocJavaList = declsAndDoc.getAsJavaList();
			declsAndDocJavaList.add(result);
			result = ListStruct.buildProperList(declsAndDocJavaList);
		}
		result = new ConsStruct(parameterList, result);
		result = setTypeSymbol(result);

		ListStruct quotedFunctionName = ListStruct.buildProperList(SpecialOperator.QUOTE, functionName);

		result = ListStruct.buildProperList(quotedFunctionName, result);
		final SymbolStruct<?> setSymbolFunction = GlobalPackageStruct.COMMON_LISP.intern("SET-SYMBOL-FUNCTION").getSymbolStruct();
		result = new ConsStruct(setSymbolFunction, result);

		final SymbolStruct<?> setFunctionField = GlobalPackageStruct.SYSTEM.intern("SET-FUNCTION-FIELD").getSymbolStruct();
		ListStruct setField = ListStruct.buildProperList(setFunctionField, quotedFunctionName);

		// null out the extra list so that it's garbage
		declsAndDoc = NullStruct.INSTANCE;

		ListStruct theList = ListStruct.buildProperList(SpecialOperator.PROGN, result, setField, quotedFunctionName);

		return theList;
	}

	protected ListStruct setTypeSymbol(ListStruct mostlyLambda) {
		return new ConsStruct(SpecialOperator.LAMBDA, mostlyLambda);
	}

	/**
	 * This method is handed the rest of a function definition following the parameter list.
	 * If there are any declarations or a doc string, the list of them is stored in the
	 * declsAndDoc instance variable. The rest of the list - which constitutes the function
	 * body is returned.
	 */
	private ListStruct processDeclsOrString(ListStruct maybeDeclsBody) {
		LispStruct firstDecls = maybeDeclsBody.getFirst();

		if ((firstDecls instanceof ListStruct) && (((ListStruct) firstDecls).getFirst() == SpecialOperator.DECLARE)) {
			declsAndDoc = new ConsStruct(firstDecls, declsAndDoc);
			return processDeclsOrString(maybeDeclsBody.getRest());

		} else if (firstDecls instanceof CharSequence) {
			// check to see if there's anything following the string. If so, it's
			// a doc string. If not, it's the body form of the function
			if (maybeDeclsBody.getRest() == NullStruct.INSTANCE) {
				return maybeDeclsBody;
			} else {
				declsAndDoc = new ConsStruct(firstDecls, declsAndDoc);
				return processDeclsOnly(maybeDeclsBody.getRest());
			}
		} else {
			return maybeDeclsBody;
		}
	}

	/**
	 * This method is handed the rest of a function definition following the parameter list.
	 * If there are any declarations, the list of them is stored in the
	 * declsAndDoc instance variable. The rest of the list - which constitutes the function
	 * body is returned. This is called only when a doc string has been previously detected.
	 */
	private ListStruct processDeclsOnly(ListStruct maybeDeclsBody) {
		LispStruct firstDecls = maybeDeclsBody.getFirst();

		if ((firstDecls instanceof ListStruct) && (((ListStruct) firstDecls).getFirst() == SpecialOperator.DECLARE)) {
			declsAndDoc = new ConsStruct(firstDecls, declsAndDoc);
			return processDeclsOnly(maybeDeclsBody.getRest());
		} else {
			return maybeDeclsBody;
		}
	}

	/*
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
	 */
}
