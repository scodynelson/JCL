package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.MacroFunctionReader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.syntax.CharacterConstants;

/**
 * Implements the ')' Lisp reader macro.
 */
public class RightParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.RIGHT_PARENTHESIS;
		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
