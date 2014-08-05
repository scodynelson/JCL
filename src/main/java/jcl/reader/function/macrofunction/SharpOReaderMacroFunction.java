package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.RationalReader;
import jcl.reader.impl.Reader;
import jcl.reader.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#o' Lisp reader macro.
 */
public class SharpOReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_O) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_O);

		final RationalReader macroFunctionReader = new RationalReader(reader);
		return macroFunctionReader.readRationalToken(BigInteger.valueOf(8));
	}
}
