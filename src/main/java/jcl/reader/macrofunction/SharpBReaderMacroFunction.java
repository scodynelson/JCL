package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#b' Lisp reader macro.
 */
public class SharpBReaderMacroFunction extends RationalReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		return process(reader, BigInteger.valueOf(2));
	}
}
