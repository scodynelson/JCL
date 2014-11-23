package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the '#+' Lisp reader macro.
 */
public final class SharpPlusSignReaderMacroFunction extends FeaturesReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpPlusSignReaderMacroFunction INSTANCE = new SharpPlusSignReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpPlusSignReaderMacroFunction() {
		super(false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.PLUS_SIGN;

		readFeatures(reader);
		return null;
	}
}
