/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the '#o' Lisp reader macro.
 */
public final class SharpOReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 8;

	public SharpOReaderMacroFunction() {
		super("SHARP-O");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_O) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_O);

		return RationalReaderMacroFunction.readRational(inputStreamStruct, RADIX);
	}
}
