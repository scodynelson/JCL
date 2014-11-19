package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#-' Lisp reader macro.
 */
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		final FeaturesReader macroFunctionReader = new FeaturesReader(reader, true);
		macroFunctionReader.process();
		return null;
	}
}
