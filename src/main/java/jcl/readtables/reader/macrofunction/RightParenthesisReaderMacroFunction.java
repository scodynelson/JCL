package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the ')' Lisp reader macro.
 */
public class RightParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.RIGHT_PARENTHESIS;
		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
