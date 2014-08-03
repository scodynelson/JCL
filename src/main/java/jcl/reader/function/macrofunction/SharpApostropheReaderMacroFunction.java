package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.variables.ReadSuppressVariable;
import jcl.reader.impl.Reader;
import jcl.structs.symbols.SpecialOperator;
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
