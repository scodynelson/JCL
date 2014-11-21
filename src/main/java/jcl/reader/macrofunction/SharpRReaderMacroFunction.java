package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the '#r' Lisp reader macro.
 */
public final class SharpRReaderMacroFunction extends RationalReaderMacroFunction {

	public static final SharpRReaderMacroFunction INSTANCE = new SharpRReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpRReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);

		return readRational(reader, numArg);
	}
}
