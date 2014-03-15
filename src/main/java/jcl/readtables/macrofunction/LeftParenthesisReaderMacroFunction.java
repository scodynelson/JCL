package jcl.readtables.macrofunction;

import jcl.readtables.state.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.LispStruct;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public class LeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;
		return reader.readList();
	}
}
