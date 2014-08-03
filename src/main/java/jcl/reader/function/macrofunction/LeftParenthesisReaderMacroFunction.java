package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.ListReader;
import jcl.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public class LeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListReader macroFunctionReader = new ListReader(reader);
		return macroFunctionReader.readList();
	}
}
