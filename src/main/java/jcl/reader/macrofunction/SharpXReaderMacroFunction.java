/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the '#x' Lisp reader macro.
 */
public final class SharpXReaderMacroFunction extends RationalReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpXReaderMacroFunction INSTANCE = new SharpXReaderMacroFunction();

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 16;

	/**
	 * Private constructor.
	 */
	private SharpXReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_X) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_X);

		return readRational(reader, BigInteger.valueOf(RADIX));
	}
}
