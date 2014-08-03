package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.FeaturesReader;
import jcl.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#-' Lisp reader macro.
 */
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		final FeaturesReader macroFunctionReader = new FeaturesReader(reader);
		macroFunctionReader.readFeatures(true);
		return null;
	}
}