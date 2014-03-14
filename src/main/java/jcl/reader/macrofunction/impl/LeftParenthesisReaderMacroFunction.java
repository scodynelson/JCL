package jcl.reader.macrofunction.impl;

import jcl.reader.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
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
