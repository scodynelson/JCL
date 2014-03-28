package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.readtables.reader.Reader;
import jcl.symbols.SymbolStruct;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;

/**
 * Implements the '#'' Lisp reader macro.
 */
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		final SymbolStruct<?> quoteToken = SpecialOperatorSymbolTokens.FUNCTION;
		return ListStruct.buildProperList(quoteToken, expression);
	}
}
