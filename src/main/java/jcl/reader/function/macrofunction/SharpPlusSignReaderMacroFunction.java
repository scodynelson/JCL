package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.function.FeaturesReader;
import jcl.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#+' Lisp reader macro.
 */
public class SharpPlusSignReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.PLUS_SIGN;

		final FeaturesReader macroFunctionReader = new FeaturesReader(reader);
		macroFunctionReader.readFeatures(false);
		return null;
	}
}
