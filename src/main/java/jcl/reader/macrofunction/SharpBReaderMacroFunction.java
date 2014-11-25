/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the '#b' Lisp reader macro.
 */
public final class SharpBReaderMacroFunction extends RationalReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpBReaderMacroFunction INSTANCE = new SharpBReaderMacroFunction();

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 2;

	/**
	 * Private constructor.
	 */
	private SharpBReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		return readRational(reader, BigInteger.valueOf(RADIX));
	}
}
