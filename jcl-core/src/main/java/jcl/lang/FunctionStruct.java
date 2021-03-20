package jcl.lang;

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
		if (functionDesignator instanceof SymbolStruct) {
			final SymbolStruct functionSymbol = (SymbolStruct) functionDesignator;

			FunctionStruct function = null;
			if (functionSymbol.hasFunction()) {
				function = functionSymbol.getFunction();
			}
			if (function == null) {
				function = functionSymbol.getMacroFunctionExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getSymbolMacroExpander();
			}
			if (function == null) {
				function = functionSymbol.getCompilerMacroFunctionExpander();
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
