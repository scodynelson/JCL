package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.function.ListReader;
import jcl.readtables.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public class LeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListReader macroFunctionReader = new ListReader(reader);
		return macroFunctionReader.readList();
	}
}