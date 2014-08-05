package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.impl.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;

/**
 * Implements the ''' Lisp reader macro.
 */
public class ApostropheReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return ListStruct.buildProperList(SpecialOperator.QUOTE, expression);
	}
}
