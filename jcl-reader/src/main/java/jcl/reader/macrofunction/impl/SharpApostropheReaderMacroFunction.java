package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.reader.tokens.SpecialOperatorSymbolTokens;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conses.ListStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.Variable;

/**
 * Implements the '#'' Lisp reader macro.
 */
public class SharpApostropheReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (Variable.ReadSuppress) {
			return null;
		} else if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		} else {
			final SymbolStruct<?> quoteToken = SpecialOperatorSymbolTokens.FUNCTION;
			return ListStruct.getStruct(quoteToken, expression);
		}
	}
}
