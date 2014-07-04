package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the ')' Lisp reader macro.
 */
public class RightParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.RIGHT_PARENTHESIS;
		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
