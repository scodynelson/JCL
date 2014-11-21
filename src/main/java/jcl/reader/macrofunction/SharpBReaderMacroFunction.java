package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the '#b' Lisp reader macro.
 */
public final class SharpBReaderMacroFunction extends RationalReaderMacroFunction {

	public static final SharpBReaderMacroFunction INSTANCE = new SharpBReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpBReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		return readRational(reader, BigInteger.valueOf(2));
	}
}
