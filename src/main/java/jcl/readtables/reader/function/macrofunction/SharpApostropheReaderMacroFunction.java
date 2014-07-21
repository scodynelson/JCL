package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.readtables.reader.ReadSuppressVariable;
import jcl.readtables.reader.impl.Reader;
import jcl.symbols.SpecialOperator;
import jcl.syntax.CharacterConstants;

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

		return ListStruct.buildProperList(SpecialOperator.FUNCTION, expression);
	}
}
