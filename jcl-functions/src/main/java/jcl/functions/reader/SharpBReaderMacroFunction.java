/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the '#b' Lisp reader macro.
 */
public final class SharpBReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 2;

	public SharpBReaderMacroFunction() {
		super("SHARP-B");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_B) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_B);

		return RationalReaderMacroFunction.readRational(inputStreamStruct, RADIX);
	}
}
