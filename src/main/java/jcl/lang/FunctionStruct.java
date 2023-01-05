package jcl.lang;

import jcl.compiler.environment.Environment;
import jcl.lang.condition.exception.TypeErrorException;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public interface FunctionStruct extends LispStruct {

	LispStruct apply(final LispStruct... lispStructs);

	SymbolStruct getFunctionSymbol();

	static FunctionStruct toLispFunction(final LispStruct functionDesignator) {
		if (functionDesignator instanceof FunctionStruct) {
			return (FunctionStruct) functionDesignator;
		}
		if (functionDesignator instanceof final SymbolStruct functionSymbol) {

			// TODO: using global environment here???

			FunctionStruct function = null;
			if (Environment.NULL.hasFunction(functionSymbol)) {
				function = Environment.NULL.getFunction(functionSymbol);
			}
			if (function == null) {
				function = Environment.NULL.getMacroFunctionExpander(functionSymbol);
			}
			if (function != null) {
				return function;
			}
		}
		if (functionDesignator instanceof ListStruct) {
			final SymbolStruct functionSymbol
					= (SymbolStruct) ((ListStruct) ((ListStruct) functionDesignator).cdr()).car();
			return toLispFunction(functionSymbol);
		}
		throw new TypeErrorException("Type cannot be converted to FUNCTION.");
	}
}
