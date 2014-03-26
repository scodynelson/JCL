package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.impl.macrofunctions.ListMacroFunctionReader;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public class LeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final StateReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListMacroFunctionReader macroFunctionReader = new ListMacroFunctionReader(reader);
		return macroFunctionReader.readList();
	}
}
