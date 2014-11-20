package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;

import java.math.BigInteger;

/**
 * Implements the ')' Lisp reader macro.
 */
public final class RightParenthesisReaderMacroFunction extends ReaderMacroFunction {

	public static final RightParenthesisReaderMacroFunction INSTANCE = new RightParenthesisReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private RightParenthesisReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.RIGHT_PARENTHESIS;
		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
