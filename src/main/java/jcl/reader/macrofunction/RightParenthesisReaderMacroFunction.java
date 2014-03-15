package jcl.reader.macrofunction;

import jcl.reader.state.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

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
