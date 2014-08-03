package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.RationalReader;
import jcl.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#b' Lisp reader macro.
 */
public class SharpBReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		final RationalReader macroFunctionReader = new RationalReader(reader);
		return macroFunctionReader.readRationalToken(BigInteger.valueOf(2));
	}
}
