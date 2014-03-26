package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.symbols.SymbolStruct;
import jcl.syntax.CharacterConstants;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the ''' Lisp reader macro.
 */
public class ApostropheReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		final SymbolStruct<?> quoteToken = SpecialOperatorSymbolTokens.QUOTE;
		return ListStruct.buildProperList(quoteToken, expression);
	}
}
