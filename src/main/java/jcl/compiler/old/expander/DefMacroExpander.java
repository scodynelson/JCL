package jcl.compiler.old.expander;

import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

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

	protected SymbolStruct macroLambdaSymbol = SpecialOperator.MACRO_LAMBDA;

	@Override
	protected ListStruct setTypeSymbol(ListStruct mostlyLambda) {
		return new ConsStruct(macroLambdaSymbol, mostlyLambda);
	}
}
