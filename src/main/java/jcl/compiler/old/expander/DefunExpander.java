package jcl.compiler.old.expander;

import jcl.LispStruct;
import jcl.compiler.old.functions.AppendFunction;
import jcl.compiler.old.symbol.DeclarationOld;
import jcl.compiler.old.symbol.SpecialOperatorOld;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

/**
 * DefunExpander takes two arguments.  The first is the list containing the Defun symbol,
 * the name of the function, the list of its parameters, and the body of the function.
 * The second argument is the environment.
 * <p>
 * If DefunExpander receives the list (defun foo (x) (+ x 7)),
 * it will return (PROGN (SETSYMBOLFUNCTION (QUOTE FOO) (LAMBDA (X) (+ X 7))) (QUOTE FOO))
 * <p>
 * The call to DefunExpander would look like this : (%DEFUN-EXPANDER (LIST 'DEFUN 'FOO '(X) '(+ X 7)) '(ENV))
 */
public class DefunExpander implements MacroFunctionExpander {

	public static final DefunExpander FUNCTION = new DefunExpander();

	//build necessary symbols for defun : SET-SYMBOL-FUNCTION, QUOTE, LAMBDA.
	protected SymbolStruct quoteSymbol = SpecialOperatorOld.QUOTE;
	protected SymbolStruct setSymbolFunction = GlobalPackageStruct.COMMON_LISP.intern("SET-SYMBOL-FUNCTION").getSymbolStruct();
	protected SymbolStruct lambdaSymbol = SpecialOperatorOld.LAMBDA;
	protected SymbolStruct prognSymbol = SpecialOperatorOld.PROGN;
	protected SymbolStruct blockSymbol = SpecialOperatorOld.BLOCK;
	protected SymbolStruct lispName = DeclarationOld.LISP_NAME;
	protected SymbolStruct setFunctionField = GlobalPackageStruct.SYSTEM.intern("SET-FUNCTION-FIELD").getSymbolStruct();
	// place to hang onto declarations and a doc string
	private ListStruct declsAndDoc = NullStruct.INSTANCE;
	private SymbolStruct declareSymbol = SpecialOperatorOld.DECLARE;

	// expands the defun macro
	@SuppressWarnings("unchecked")
	public LispStruct expand(Object form, Object env) {
		ListStruct listToExpand = (ListStruct) form;
		// (defun foo (args) (decls or doc...) theBody)
//        Function1 printFn = (Function1)lisp.common.function.Print.FUNCTION;

		//sets functionName to the name of the function passed in the list.
		ListStruct listCdr = listToExpand.getRest();
		// (foo (args) (decls or doc...) theBody))
		SymbolStruct functionName = (SymbolStruct) listCdr.getFirst();

		// create the function name declaration
		ListStruct nameDeclareClause = ListStruct.buildProperList(lispName, functionName);
		ListStruct nameDeclaration = ListStruct.buildProperList(declareSymbol, nameDeclareClause);

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
		result = new ConsStruct(blockSymbol, result);
		result = new ConsStruct(result, NullStruct.INSTANCE);
		result = new ConsStruct(nameDeclaration, result);
		if (declsAndDoc != NullStruct.INSTANCE) {
			result = (ListStruct) AppendFunction.FUNCTION.funcall(declsAndDoc, result);
		}
		result = new ConsStruct(parameterList, result);
		result = setTypeSymbol(result);

		ListStruct quotedFunctionName = ListStruct.buildProperList(quoteSymbol, functionName);

		result = ListStruct.buildProperList(quotedFunctionName, result);
		result = new ConsStruct(setSymbolFunction, result);

		ListStruct setField = ListStruct.buildProperList(setFunctionField, quotedFunctionName);

		// null out the extra list so that it's garbage
		declsAndDoc = NullStruct.INSTANCE;

		ListStruct theList = ListStruct.buildProperList(prognSymbol, result, setField, quotedFunctionName);

		return theList;
	}

	protected ListStruct setTypeSymbol(ListStruct mostlyLambda) {
		return new ConsStruct(lambdaSymbol, mostlyLambda);
	}

	/**
	 * This method is handed the rest of a function definition following the parameter list.
	 * If there are any declarations or a doc string, the list of them is stored in the
	 * declsAndDoc instance variable. The rest of the list - which constitutes the function
	 * body is returned.
	 */
	private ListStruct processDeclsOrString(ListStruct maybeDeclsBody) {
		LispStruct firstDecls = maybeDeclsBody.getFirst();

		if ((firstDecls instanceof ListStruct) && (((ListStruct) firstDecls).getFirst() == declareSymbol)) {
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

		if ((firstDecls instanceof ListStruct) && (((ListStruct) firstDecls).getFirst() == declareSymbol)) {
			declsAndDoc = new ConsStruct(firstDecls, declsAndDoc);
			return processDeclsOnly(maybeDeclsBody.getRest());
		} else {
			return maybeDeclsBody;
		}
	}
}
