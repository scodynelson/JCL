package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public interface FunctionStruct extends LispStruct {

	LispStruct apply(final LispStruct... lispStructs);

	SymbolStruct getFunctionSymbol();

	static FunctionStruct toLispFunction(final LispStruct functionDesignator) {
		// TODO: probably a better way to do this without returning "null"
		FunctionStruct function = null;
		if (functionDesignator instanceof SymbolStruct) {
			final SymbolStruct functionSymbol = (SymbolStruct) functionDesignator;
			if (functionSymbol.hasFunction()) {
				function = functionSymbol.getFunction();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getMacroFunctionExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getSymbolMacroExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getCompilerMacroFunctionExpander();
			}
		} else if (functionDesignator instanceof ListStruct) {
			final SymbolStruct functionSymbol
					= (SymbolStruct) ((ListStruct) ((ListStruct) functionDesignator).cdr()).car();
			if (functionSymbol.hasFunction()) {
				function = functionSymbol.getFunction();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getMacroFunctionExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getSymbolMacroExpander();
			}
			if (function == null) {
				function = (FunctionStruct) functionSymbol.getCompilerMacroFunctionExpander();
			}
		} else if (functionDesignator instanceof FunctionStruct) {
			return  (FunctionStruct) functionDesignator;
		} else {
			throw new TypeErrorException("Unsupported Function Designator.");
		}
		return function;
	}
}
