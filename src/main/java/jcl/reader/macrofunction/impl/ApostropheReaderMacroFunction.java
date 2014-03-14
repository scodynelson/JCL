package jcl.reader.macrofunction.impl;

import jcl.reader.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.reader.tokens.SpecialOperatorSymbolTokens;
import jcl.LispStruct;
import jcl.structs.ListStruct;
import jcl.structs.SymbolStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.Variable;

/**
 * Implements the ''' Lisp reader macro.
 */
public class ApostropheReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (Variable.ReadSuppress) {
			return null;
		}

		if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		final SymbolStruct<?> quoteToken = SpecialOperatorSymbolTokens.QUOTE;
		return ListStruct.buildProperList(quoteToken, expression);
	}
}
